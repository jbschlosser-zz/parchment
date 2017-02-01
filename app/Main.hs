{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick.AttrMap (attrMap)
import Brick.Main (App(..), customMain, showFirstCursor, halt, continue)
import Brick.Markup (markup)
import Brick.Types (Widget(..), Padding(..), Location(..), Next, EventM, BrickEvent(..),
                    getContext, Size(..), availHeightL)
import Brick.Widgets.Core ((<=>), padBottom, str, vBox, showCursor)
import Brick.Widgets.Border (hBorder)
import Conduit
import Control.Concurrent (forkIO, newChan, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM.TQueue
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TQueue (sourceTQueue)
import Data.Default
import Data.Map (fromList)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Text (singleton)
import Data.Text.Markup ((@@))
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Lens.Micro ((^.), (&), (.~))
import Network (withSocketsDo)
import Parchment.FString
import Parchment.Session
import ScriptInterface
import System.Console.ArgParser
import System.Console.ArgParser.Format
import qualified System.Console.Terminal.Size as T
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir

data RecvEvent = RecvEvent BS.ByteString

data AppArgs = AppArgs String Int deriving (Show)
argParser :: ParserSpec AppArgs
argParser = AppArgs
    `parsedBy` optPos "127.0.0.1" "hostname" `Descr` "Hostname of the MUD server"
    `andBy` optPos 4000 "port" `Descr` "Port of the MUD server"

withCorrectArgsDo :: (AppArgs -> IO ()) -> IO ()
withCorrectArgsDo app = do
    args <- getArgs
    interface <- (`setAppDescr` "Haskell MUD client") <$> mkApp argParser
    let parse_result = parseArgs args interface
    case parse_result of
         Left err -> putStrLn $ showCmdLineAppUsage defaultFormat interface ++ ['\n'] ++ err
         Right args -> app args

-- Main function.
main :: IO ()
main = withSocketsDo . withCorrectArgsDo $ \args -> do
    let AppArgs hostname port = args
    send_queue <- newTQueueIO
    eventChan <- newChan
    (scmEnv, configErr) <- loadConfig
    let sess = (case configErr of
                    Just err -> writeBufferLn . colorize V.red $
                        "Config error: " ++ err
                    Nothing -> id) $ initialSession send_queue keyBindings scmEnv
    forkIO $ runTCPClient (clientSettings port (BSC.pack hostname)) $ \server ->
        void $ concurrently
            (appSource server $$ chanSink eventChan chanWriteRecvEvent (return . const ()))
            (sourceTQueue send_queue $$ appSink server)
    void . customMain (V.mkVty def) (Just eventChan) app $ sess
    where
        chanSink ch writer closer = do
            CL.mapM_ $ liftIO . writer ch
            liftIO $ closer ch
        chanWriteRecvEvent c s = writeChan c (RecvEvent s)

-- Loads the config file. Returns the environment and optionally any errors.
loadConfig :: IO (Env, Maybe String)
loadConfig = do
    scmEnv <- scriptInterface
    configPath <- getUserConfigFile "parchment" "config.scm"
    let conf = List [Atom "include", String configPath]
    res <- evalLisp' scmEnv conf
    case res of
         Left err -> return (scmEnv, Just $ show err)
         Right _ -> return (scmEnv, Nothing)

-- Application setup.
app :: App Sess RecvEvent ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = onAppStart
        , appChooseCursor = showFirstCursor
        }

-- Get initial number of lines available to the buffer during startup.
-- This info is needed for proper scrollback. Note that the number is updated
-- during every resize to remain correct.
onAppStart :: Sess -> EventM () Sess
onAppStart sess = do
    size <- liftIO T.size
    let lines = (T.height $ fromJust size) - nonBufferLines
    return $ sess & buf_lines .~ lines

-- Key bindings.
keyBindings = fromList $ map rawKeyBinding rawKeys ++
    [ ((V.EvKey V.KEsc []), halt)
    , ((V.EvKey V.KBS []), continue . delKey)
    , ((V.EvKey V.KEnter []), \sess -> do
        let input = getInput sess
        let to_eval = List [Atom "send-hook", String input]
        res <- liftIO $ evalLisp' (_scm_env sess) to_eval
        case res of
             Right l -> do
                 case l of
                      Opaque _ -> do
                          res <- liftIO $ opaqueToAction l sess
                          case res of
                               Nothing -> halt sess
                               Just s -> continue s
                      x -> continue $ flip writeBufferLn sess $
                           colorize V.red $ "Expected an action from send-hook, found: " ++
                               (show x)
             Left err -> continue $ flip writeBufferLn sess $ colorize V.red $ show err)
    , ((V.EvKey V.KPageUp []), continue . pageUp)
    , ((V.EvKey V.KPageDown []), continue . pageDown)
    , ((V.EvKey V.KUp []), continue . historyOlder)
    , ((V.EvKey V.KDown []), continue . historyNewer)
    , ((V.EvKey (V.KChar 'u') [V.MCtrl]), continue . clearInputLine)
    ]
    where
        rawKeys = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"
        rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey c st)


-- Handle UI and other app events.
handleEvent :: Sess -> BrickEvent () RecvEvent -> EventM () (Next Sess)
handleEvent sess (VtyEvent e) =
    case Map.lookup e (_bindings sess) of
        Just b -> b sess
        Nothing -> case e of
                        -- Update the number of buffer lines after the resize.
                        V.EvResize _ lines -> continue $
                            scrollLines 0 (sess & buf_lines .~ (lines - nonBufferLines))
                        -- No binding was found.
                        _ -> continue $ flip writeBufferLn sess $
                            colorize V.magenta $ "No binding found: " ++ show e
handleEvent sess (AppEvent e) =
    case e of
        RecvEvent bs -> continue $ receiveServerData sess bs
handleEvent sess _ = continue sess

-- Draw the UI.
drawUI :: Sess -> [Widget()]
drawUI sess =
    [vBox [ padBottom Max $ drawBuffer buf scroll
          , hBorder
          , showCursor () (Location (curs, 0))
              (if length input > 0 then str input else str " ")
          ]]
    where input = getInput sess
          buf = sess ^. buffer
          curs = sess ^. cursor
          scroll = sess ^. scroll_loc

-- A bit hackish.. this is the number of vertical lines in the interface
-- that aren't reserved for the buffer.
nonBufferLines :: Int
nonBufferLines = 2

drawBuffer :: [FString] -> Int -> Widget()
drawBuffer lines scroll = 
    Widget Greedy Greedy $ do
        ctx <- getContext
        let num = ctx ^. availHeightL
        let start = max 0 $ length lines - scroll - num
        render $ foldr (<=>) (str "") $ map drawBufferLine $ take num $ drop start lines
    where drawBufferLine [] = str " " -- handle blank case
          drawBufferLine s = markup . mconcat . map fcharToMarkup $ s
          fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)
