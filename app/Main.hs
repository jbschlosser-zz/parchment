{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Brick.AttrMap (attrMap)
import Brick.Main (App(..), customMain, showFirstCursor, halt, continue)
import Brick.Markup (markup)
import Brick.Types (Widget(..), Padding(..), Location(..), Next, EventM, BrickEvent(..),
                    getContext, Size(..), availHeightL, availWidthL)
import Brick.Widgets.Core ((<=>), padBottom, str, hBox, vBox, showCursor)
import Conduit
import Control.Concurrent (forkIO, newChan, writeChan)
import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Control.Concurrent.STM.TQueue
import Control.Monad (void)
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
import qualified Data.Sequence as S
import qualified Data.Text as TXT
import Data.Text.Markup ((@@))
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Lens.Micro ((^.), (&), (.~))
import Network (withSocketsDo)
import Parchment.FString
import qualified Parchment.RingBuffer as RB
import Parchment.Session
import Parchment.Telnet
import Parchment.Util
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
    let settings = defaultSettings hostname port
    sess <- runIOMaybe . loadConfigAction $
        initialSession settings send_queue keyBindings scmEnv
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
    , ((V.EvKey V.KBS []), continue . backspaceInput)
    , ((V.EvKey V.KDel []), continue . deleteInput)
    , ((V.EvKey V.KEnter []), \sess -> do
        let input = getInput sess
        let to_eval = List [Atom "send-hook", String input]
        res <- liftIO $ evalLisp' (sess ^. scm_env) to_eval
        let sesh = clearInput . historyNewest $ sess
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
    , ((V.EvKey V.KLeft []), continue . moveCursor (-1))
    , ((V.EvKey V.KRight []), continue . moveCursor 1)
    , ((V.EvKey (V.KChar 'u') [V.MCtrl]), continue . clearInput)
    ]
    where
        rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addInput c st)

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
            -- Receive and process the server data.
            let sess2 = receiveServerData sess bs
            -- Extract received text and telnet commands.
            let recv_text = reverse $ sess2 ^. recv_state ^. text
            let telnets = sess2 ^. recv_state ^. telnet_cmds
            -- Reset received text and telnet commands for next time.
            let sess3 = sess2 & (recv_state . text) .~ []
                              & (recv_state . telnet_cmds) .~ []
            -- Write received text to the buffer.
            let sess4 = writeBuffer recv_text sess3
            -- Call the recv hook.
            let to_eval = List [Atom "recv-hook", String $
                                 filter (not . (==) '\r') $ removeFormatting recv_text]
            res <- liftIO $ evalLisp' (sess4 ^. scm_env) to_eval
            let action = case res of
                    Right l -> do
                        case l of
                            Opaque _ -> opaqueToAction l
                            x -> return . (writeBufferLn $
                                colorize V.red $ "Expected an action, found: " ++ (show x))
                    Left err -> return . (writeBufferLn (colorize V.red $ show err))
            res2 <- liftIO . runIOMaybe $ action sess4
            case res2 of
                Nothing -> halt sess4
                Just sess5 -> do
                    -- Handle telnet commands.
                    let handlers = map (handleTelnet . BS.unpack) telnets
                    sess6 <- liftIO . runIOMaybe $ (chainM handlers) sess5
                    case sess6 of
                        Nothing -> halt sess5
                        Just sess7 -> continue sess7
handleEvent sess _ = continue sess

handleTelnet :: [Word8] -> Sess -> IOMaybe Sess
handleTelnet cmd
    | cmd == [tIAC, tDO, tTELETYPE] = liftIO . sendRawToServer [tIAC, tWONT, tTELETYPE]
    -- Probably not a good idea to send this..
    -- | cmd == [tIAC, tSB, tTELETYPE, tSEND, tIAC, tSE] = sendRawToServer $
    --     [tIAC, tSB, tTELETYPE, tIS] ++ (BS.unpack . BSC.pack $ "parchment") ++ [tIAC, tSE]
    | cmd == [tIAC, tDONT, tNAWS] = liftIO . sendRawToServer [tIAC, tWONT, tNAWS]
    | cmd == [tIAC, tWILL, tGMCP] = liftIO . sendRawToServer [tIAC, tDO, tGMCP]
    | [tIAC, tSB, tGMCP] `isPrefixOf` cmd =
        handleGmcp (BSC.unpack . BS.pack . leave 2 . drop 3 $ cmd)
    | cmd == [tIAC, tNOP] = return
    -- TODO: Support these.
    | cmd == [tIAC, tDO, tNAWS] = liftIO . sendRawToServer [tIAC, tWONT, tNAWS]
    | cmd == [tIAC, tWILL, tMXP] = liftIO . sendRawToServer [tIAC, tDONT, tMXP]
    | cmd == [tIAC, tWILL, tMCCP2] = liftIO . sendRawToServer [tIAC, tDONT, tMCCP2]
    | cmd == [tIAC, tWILL, tMSSP] = liftIO . sendRawToServer [tIAC, tDONT, tMSSP]
    | cmd == [tIAC, tWILL, tMSDP] = liftIO . sendRawToServer [tIAC, tDONT, tMSDP]
    | otherwise = return . writeBufferLn (colorize V.red $ show cmd)

handleGmcp :: String -> Sess -> IOMaybe Sess
handleGmcp cmd sess = do
    let to_eval = List [Atom "gmcp-hook", String cmd]
    res <- liftIO $ evalLisp' (sess ^. scm_env) to_eval
    case res of
        Right l -> do
            case l of
                Opaque _ -> opaqueToAction l sess
                x -> return . flip writeBufferLn sess $
                    colorize V.red $ "Expected an action, found: " ++ (show x)
        Left err -> return . flip writeBufferLn sess $ colorize V.red $ show err

-- Draw the UI.
drawUI :: Sess -> [Widget()]
drawUI sess =
    -- TODO: Make this more efficient with format strings or Data.Text or something.
    [vBox [ drawStatusLine $ "parchment [" ++ (sess ^. (settings . hostname)) ++
                ":" ++ show (sess ^. (settings . port)) ++ "]"
          , padBottom Max $ drawBuffer buf scroll
          , drawStatusLine ""
          , hBox [ str "> "
                 , showCursor () (Location (curs, 0))
                       (if length input > 0 then str input else str " ")
                 ]

          ]]
    where input = getInput sess
          buf = sess ^. buffer
          curs = sess ^. cursor
          scroll = sess ^. scroll_loc

-- A bit hackish.. this is the number of vertical lines in the interface
-- that aren't reserved for the buffer.
nonBufferLines :: Int
nonBufferLines = 3 -- one for the input line, one each for top/bottom status lines

drawBuffer :: RB.RingBuffer FString -> Int -> Widget()
drawBuffer buf scroll =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let num_lines = ctx ^. availHeightL
        render $ foldr (<=>) (str "") . fmap drawBufferLine .
            S.reverse . S.take num_lines . RB.drop scroll $ buf
    where fcharToMarkup fc = (TXT.singleton $ _ch fc) @@ (_attr fc)
          drawBufferLine [] = str " " -- handle blank case
          drawBufferLine fs = markup . mconcat . fmap fcharToMarkup $ fs

drawStatusLine :: String -> Widget()
drawStatusLine line =
    Widget Greedy Fixed $ do
        ctx <- getContext
        let width = ctx ^. availWidthL
        let spaces = max 0 $ width - length line
        let full_line = line ++ (take spaces $ repeat ' ')
        let formatted = TXT.pack full_line @@
                            ( flip V.withForeColor V.white
                            .  flip V.withBackColor (V.rgbColor 0 95 135) $ V.defAttr)
        render . markup $ formatted
