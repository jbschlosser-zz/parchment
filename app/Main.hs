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
import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Control.Concurrent.STM.TQueue
import Control.Monad (void, (>=>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.TQueue (sourceTQueue)
import Data.Default
import Data.List (isPrefixOf)
import Data.Map (fromList)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Data.Text (singleton)
import Data.Text.Markup ((@@))
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Lens.Micro ((^.), (&), (.~))
import Network (withSocketsDo)
import Parchment.FString
import Parchment.Session
import Parchment.Telnet
import ScriptInterface
import System.Console.ArgParser
import System.Console.ArgParser.Format
import qualified System.Console.Terminal.Size as T
import System.Environment (getArgs)

data AppEvent = RecvEvent BS.ByteString

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
    event_chan <- newChan
    scmEnv <- r5rsEnv
    sess <- loadConfigAction $ initialSession send_queue keyBindings scmEnv
    case sess of
         Nothing -> return ()
         Just sess -> do
            forkIO $ runTCPClient (clientSettings port (BSC.pack hostname)) $ \server ->
                void $ runConcurrently $ (,,)
                    <$> Concurrently (appSource server $$ chanSink event_chan
                                     chanWriteRecvEvent (return . const ()))
                    <*> Concurrently (sourceTQueue send_queue $$ appSink server)
            void . customMain (V.mkVty def) (Just event_chan) app $ sess
    where
        chanSink ch writer closer = do
            CL.mapM_ $ liftIO . writer ch
            liftIO $ closer ch
        chanWriteRecvEvent c s = writeChan c (RecvEvent s)

-- Application setup.
app :: App Sess AppEvent ()
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
        let sesh = clearInputLine . historyNewest $ sess
        case res of
             Right l -> do
                 case l of
                      Opaque _ -> liftAction (opaqueToAction l) sesh
                      x -> continue $ flip writeBufferLn sesh $
                           colorize V.red $ "Expected an action, found: " ++ (show x)
             Left err -> continue $ flip writeBufferLn sesh $ colorize V.red $ show err)
    , ((V.EvKey V.KPageUp []), continue . pageUp)
    , ((V.EvKey V.KPageDown []), continue . pageDown)
    , ((V.EvKey V.KUp []), continue . historyOlder)
    , ((V.EvKey V.KDown []), continue . historyNewer)
    , ((V.EvKey (V.KChar 'u') [V.MCtrl]), continue . clearInputLine)
    ]
    where
        rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey c st)

-- Handle UI and other app events.
handleEvent :: Sess -> BrickEvent () AppEvent -> EventM () (Next Sess)
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
        RecvEvent bs -> do
            let new_sess = receiveServerData sess bs
            let handlers = map (handleTelnet . BS.unpack) (new_sess ^. telnet_cmds)
            liftIO (foldr (>=>) return handlers new_sess) >>= continue
handleEvent sess _ = continue sess

leave :: Int -> [a] -> [a]
leave n lst = take (length lst - n) lst

handleTelnet :: [Word8] -> Sess -> IO Sess
handleTelnet cmd
    | cmd == [tIAC, tDO, tTELETYPE] = sendRawToServer [tIAC, tWONT, tTELETYPE]
    -- Probably not a good idea to send this..
    -- | cmd == [tIAC, tSB, tTELETYPE, tSEND, tIAC, tSE] = sendRawToServer $
    --     [tIAC, tSB, tTELETYPE, tIS] ++ (BS.unpack . BSC.pack $ "parchment") ++ [tIAC, tSE]
    | cmd == [tIAC, tDONT, tNAWS] = sendRawToServer [tIAC, tWONT, tNAWS]
    | cmd == [tIAC, tWILL, tGMCP] = sendRawToServer [tIAC, tDO, tGMCP]
    | [tIAC, tSB, tGMCP] `isPrefixOf` cmd =
        handleGmcp (BSC.unpack . BS.pack . leave 2 . drop 3 $ cmd)
    | cmd == [tIAC, tNOP] = return
    -- TODO: Support these.
    | cmd == [tIAC, tDO, tNAWS] = sendRawToServer [tIAC, tWONT, tNAWS]
    | cmd == [tIAC, tWILL, tMXP] = sendRawToServer [tIAC, tDONT, tMXP]
    | cmd == [tIAC, tWILL, tMCCP2] = sendRawToServer [tIAC, tDONT, tMCCP2]
    | cmd == [tIAC, tWILL, tMSSP] = sendRawToServer [tIAC, tDONT, tMSSP]
    | cmd == [tIAC, tWILL, tMSDP] = sendRawToServer [tIAC, tDONT, tMSDP]
    | otherwise = return . writeBufferLn (colorize V.red $ show cmd)

handleGmcp :: String -> Sess -> IO Sess
handleGmcp cmd = return . writeBufferLn (colorize V.green cmd)

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
