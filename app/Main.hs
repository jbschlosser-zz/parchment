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
import Parchment.Fchar
import Parchment.Parsing
import Parchment.Session
import ScriptInterface
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
                      Right func -> (liftIO . func $ sess) >>= continue
                      Left err -> case l of
                                       List lst -> (liftIO . flip chainM sess $
                                                    map opaqueToSessFunc lst) >>= continue
                                       _ -> continue $ flip writeScrollbackLn sess $
                                                colorize V.red "Wrong return value"
             Left err -> continue $ flip writeScrollbackLn sess $ colorize V.red $ show err)
    ] ++ map rawKeyBinding rawKeys)

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

-- === HELPER FUNCTIONS. ===
-- Loads the config file. Returns the environment and optionally any errors.
loadConfig :: IO (Env, Maybe String)
loadConfig = do
    scmEnv <- scriptInterface
    configPath <- getUserConfigFile "parchment" "config.scm"
    configFileContents <- tryIOError $ readFile configPath
    let (conf, err) = case configFileContents of
                          Right c -> (c, Nothing)
                          Left c -> ("", Just $ "Could not load config file: " ++ configPath)
    -- TODO: Proper error handling here.
    evalString scmEnv conf
    return (scmEnv, err)

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

-- Chain a list of monad functions, with each result feeding into the next.
chainM :: (Monad m) => [a -> m a] -> a -> m a
chainM [] a = return a
chainM (x:xs) a = x a >>= chainM xs

-- Convert an opaque lisp value to a session-transforming action.
opaqueToSessFunc :: LispVal -> Sess -> IO Sess
opaqueToSessFunc lv = case fromOpaque lv of
                           Right f -> f
                           Left err -> case fromOpaque lv of
                                            Right f -> return . f
                                            Left err -> return . (writeScrollbackLn
                                                (colorize V.red $ "Error: " ++ (show err)))
