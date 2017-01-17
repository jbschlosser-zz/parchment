{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main (App(..), defaultMain, customMain, resizeOrQuit, neverShowCursor,
                   showFirstCursor, halt, continue, suspendAndResume)
import Brick.Markup (markup, (@?))
import Brick.Types (Widget(..), Padding(..), Location(..), Next(..), EventM, BrickEvent(..),
                    getContext, Size(..), Result(..), availHeightL)
import Brick.Widgets.Core ((<=>), (<+>), padLeft, padTop, padRight, padBottom, str, vBox,
                           hBox, showCursor, vLimit, fill)
import Brick.Widgets.Border (hBorder)
import Conduit
import Control.Concurrent (forkIO, newChan, Chan, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM.TQueue
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TQueue (sourceTQueue)
import Data.Default
import Data.Map (fromList)
import qualified Data.Map.Lazy as Map
import Data.Text (singleton)
import Data.Text.Markup ((@@), Markup)
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Variables
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Network (withSocketsDo)
import Parchment.Parsing
import Parchment.Session
import System.Environment.XDG.BaseDir
import System.IO.Error

data RecvEvent = RecvEvent BS.ByteString

-- Main function.
main :: IO ()
main = withSocketsDo $ void $ do
    send_queue <- newTQueueIO
    recv_queue <- newTQueueIO
    eventChan <- newChan
    (scmEnv, configErr) <- loadConfig
    let sess = (case configErr of
                    Just err -> writeScrollbackLn $ colorize V.red $
                        "Config error: " ++ err
                    Nothing -> id) $ initialSession send_queue keyBindings scmEnv
    tid <- forkIO $ runTCPClient (clientSettings 4000 (BSC.pack "127.0.0.1")) $ \server ->
        void $ concurrently
            (appSource server $$ chanSink eventChan chanWriteRecvEvent (\c -> return ()))
            (sourceTQueue send_queue $$ appSink server)
    customMain (V.mkVty def) (Just eventChan) app sess
    where
        chanWriteRecvEvent c s = writeChan c (RecvEvent s)

-- Application setup.
app :: App Sess RecvEvent ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        }

-- Key bindings.
keyBindings = fromList
    ([ ((V.EvKey V.KEsc []), halt)
    , ((V.EvKey V.KBS []), continue . delKey)
    , ((V.EvKey V.KEnter []), \sess -> do
        sess <- liftIO $ sendToServer (getInput sess) sess
        continue $ nextHistory $ writeScrollbackLn
            (colorize V.yellow $ getInput sess) sess)
    , ((V.EvKey V.KPageUp []), continue . pageUp)
    , ((V.EvKey V.KPageDown []), continue . pageDown)
    , ((V.EvKey V.KUp []), continue . historyOlder)
    , ((V.EvKey V.KDown []), continue . historyNewer)
    , ((V.EvKey (V.KChar 'u') [V.MCtrl]), continue . clearInputLine)
    , ((V.EvKey (V.KFun 12) []), \sess -> do
        let input = getInput sess
        let to_eval = List [Atom "send-hook", String input]
        res <- liftIO $ evalLisp' (_scm_env sess) to_eval
        case res of
             Right l -> do
                 case fromOpaque l of
                      Right func -> continue . func $ sess
                      Left err -> case l of
                                       List lst -> continue $ flip foldFuncList sess $
                                           map opaqueToSessFunc lst
                                       _ -> continue $ writeScrollbackLn
                                                (colorize V.red "Wrong return value") sess
             Left err -> continue $ flip writeScrollbackLn sess $ colorize V.red $ show err)
    ] ++ map rawKeyBinding rawKeys)
    where opaqueToSessFunc lv = case fromOpaque lv of
                                     Right f -> f
                                     Left err -> writeScrollbackLn (colorize V.red $
                                         "Error: " ++ (show err))

-- Handle UI and other app events.
handleEvent :: Sess -> BrickEvent () RecvEvent -> EventM () (Next Sess)
handleEvent sess (VtyEvent e) =
    case Map.lookup e (_bindings sess) of
        Just b -> b sess
        Nothing -> continue $ flip writeScrollbackLn sess $
            colorize V.magenta $ "No binding found: " ++ show e
handleEvent sess (AppEvent e) =
    case e of
        RecvEvent bs -> continue $ receiveServerData sess bs
handleEvent sess _ = continue sess

-- Draw the UI.
drawUI :: Sess -> [Widget()]
drawUI sess =
    [vBox [ padBottom Max $ drawScrollback sb scroll
          , hBorder
          , showCursor () (Location (curs, 0))
              (if length input > 0 then str input else str " ")
          ]]
    -- TODO: Fix this to use lenses.
    where input = getInput sess
          sb = _scrollback sess
          curs = _cursor sess
          scroll = _scroll_loc sess

drawScrollback :: [[Fchar]] -> Int -> Widget()
drawScrollback lines scroll = 
    Widget Greedy Greedy $ do
        ctx <- getContext
        let num = ctx ^. availHeightL
        let start = max 0 $ length lines - scroll - num
        render $ foldr (<=>) (str "") $ map drawScrollbackLine $ take num $ drop start lines
    where drawScrollbackLine [] = str " " -- handle blank case
          drawScrollbackLine s = markup . mconcat . map fcharToMarkup $ s
          fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)

-- Helper functions.
-- Loads the config file. Returns the environment and optionally any errors.
loadConfig :: IO (Env, Maybe String)
loadConfig = do
    scmEnv <- r5rsEnv >>= flip extendEnv
        [ ((varNamespace, "del-key"), toOpaque delKey)
        , ((varNamespace, "clear-input-line"), toOpaque clearInputLine)
        , ((varNamespace, "page-up"), toOpaque pageUp)
        , ((varNamespace, "page-down"), toOpaque pageDown)
        , ((varNamespace, "next-history"), toOpaque nextHistory)
        , ((varNamespace, "history-older"), toOpaque historyOlder)
        , ((varNamespace, "history-newer"), toOpaque historyNewer)
        , ((varNamespace, "scroll-history"), PrimitiveFunc scrollHistoryWrapper)
        , ((varNamespace, "scroll-lines"), PrimitiveFunc scrollLinesWrapper)
        , ((varNamespace, "print"), PrimitiveFunc writeScrollbackWrapper)
        , ((varNamespace, "println"), PrimitiveFunc writeScrollbackLnWrapper)
        , ((varNamespace, "add-key"), PrimitiveFunc addKeyWrapper)]
    configPath <- getUserConfigFile "parchment" "config.scm"
    configFileContents <- tryIOError $ readFile configPath
    let (conf, err) = case configFileContents of
                          Right c -> (c, Nothing)
                          Left c -> ("", Just $ "Could not load config file: " ++ configPath)
    -- TODO: Proper error handling here.
    evalString scmEnv conf
    return (scmEnv, err)

-- === BINDING WRAPPERS. ===
-- TODO: Find a less repetitive way to do this.
addKeyWrapper :: [LispVal] -> ThrowsError LispVal
addKeyWrapper xs | length xs == 1 =
    case head xs of
         Char c -> Right $ toOpaque $ addKey c
         _ -> Left $ Default "Expected a character"
addKeyWrapper _ = Left $ Default "Expected a character"

scrollHistoryWrapper :: [LispVal] -> ThrowsError LispVal
scrollHistoryWrapper xs | length xs == 1 =
    case head xs of
         Number i -> Right $ toOpaque $ scrollHistory $ fromIntegral i
         _ -> Left $ Default "Expected a number"
scrollHistoryWrapper _ = Left $ Default "Expected a number"

scrollLinesWrapper :: [LispVal] -> ThrowsError LispVal
scrollLinesWrapper xs | length xs == 1 =
    case head xs of
         Number i -> Right $ toOpaque $ scrollLines $ fromIntegral i
         _ -> Left $ Default "Expected a number"
scrollLinesWrapper _ = Left $ Default "Expected a number"

writeScrollbackWrapper :: [LispVal] -> ThrowsError LispVal
writeScrollbackWrapper xs | length xs == 1 =
    case fromOpaque (head xs) of
         Right fc -> Right $ toOpaque $ writeScrollback fc
         Left e -> case (head xs) of
                        String s -> Right $ toOpaque $ writeScrollback $ defaultColor s
                        _ -> Left $ Default "Expected a list of formatted chars"
writeScrollbackWrapper _ = Left $ Default "Expected a list of formatted chars"

writeScrollbackLnWrapper :: [LispVal] -> ThrowsError LispVal
writeScrollbackLnWrapper xs | length xs == 1 =
    case fromOpaque (head xs) of
         Right fc -> Right $ toOpaque $ writeScrollbackLn fc
         Left e -> case (head xs) of
                        String s -> Right $ toOpaque $ writeScrollbackLn $ defaultColor s
                        _ -> Left $ Default "Expected a list of formatted chars"
writeScrollbackLnWrapper _ = Left $ Default "Expected a list of formatted chars"

-- Creates a binding for a raw char key.
rawKeyBinding :: Char -> (V.Event, (Sess -> EventM () (Next Sess)))
rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey c st)

-- The raw keys to use.
rawKeys :: String
rawKeys = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"

-- channel, write func, close func
chanSink :: MonadIO m => chan -> (chan -> a -> IO ()) -> (chan -> IO ()) -> Sink a m ()
chanSink ch writer closer = do
    CL.mapM_ $ liftIO . writer ch
    liftIO $ closer ch
