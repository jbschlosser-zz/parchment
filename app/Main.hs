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
import qualified Data.Map.Lazy as Map
import Data.Text (singleton)
import Data.Text.Markup ((@@), Markup)
import qualified Graphics.Vty as V
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Network (withSocketsDo)
import Parchment.Parsing
import Parchment.Session

data RecvEvent = RecvEvent BS.ByteString

-- Main function.
main :: IO ()
main = withSocketsDo $ void $ do
    send_queue <- newTQueueIO
    recv_queue <- newTQueueIO
    eventChan <- newChan
    tid <- forkIO $ runTCPClient (clientSettings 4000 (BSC.pack "127.0.0.1")) $ \server ->
        void $ concurrently
            (appSource server $$ chanSink eventChan chanWriteRecvEvent (\c -> return ()))
            (sourceTQueue send_queue $$ appSink server)
    customMain (V.mkVty def) (Just eventChan) app (initialState send_queue)
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

-- Initial state of the session data.
initialState :: TQueue BS.ByteString -> Sess
initialState q =
    Sess {
        _scrollback = [[]]
        , _input = ""
        , _cursor = 0
        , _bindings = createBindings
            ([ ((V.EvKey V.KEsc []), \st -> halt st)
            , ((V.EvKey V.KBS []), \st -> continue $ delKey st)
            , ((V.EvKey V.KEnter []), \st -> do
                s <- liftIO $ sendInput st
                continue $ clearInput s)
            , ((V.EvKey V.KPageUp []), \st -> continue $ pageUp st)
            , ((V.EvKey V.KPageDown []), \st -> continue $ pageDown st)
            ] ++ map rawKeyBinding rawKeys)
        , _send_queue = q
        , _telnet_state = NotInProgress
        , _esc_seq_state = NotInProgress
        , _char_attr = V.defAttr
        , _scroll_loc = 0
        }

-- Handle UI and other app events.
handleEvent :: Sess -> BrickEvent () RecvEvent -> EventM () (Next Sess)
handleEvent st (VtyEvent e) =
    case Map.lookup e (_bindings st) of
        Just b -> b st
        Nothing -> continue st
handleEvent st (AppEvent e) =
    case e of
        RecvEvent bs -> continue $ receiveServerData st bs
handleEvent st _ = continue st

-- Draw the UI.
drawUI :: Sess -> [Widget()]
drawUI (Sess {_scrollback = sb, _input = input, _cursor = cursor, _scroll_loc = scroll}) =
    [vBox [ padBottom Max $ drawScrollback sb scroll
          , hBorder
          , showCursor () (Location (cursor, 0))
              (if length input > 0 then str input else str " ")
          ]]

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
-- Creates bindings from a list of tuples.
createBindings :: [(V.Event, (Sess -> EventM () (Next Sess)))] ->
    Map.Map V.Event (Sess -> EventM () (Next Sess))
createBindings = (mconcat . map createBinding)
    where createBinding (e, b) = Map.insert e b Map.empty

-- Creates a binding for a raw char key.
rawKeyBinding :: Char -> (V.Event, (Sess -> EventM () (Next Sess)))
rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey st c)

-- The raw keys to use.
rawKeys :: String
rawKeys = "abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"

-- channel, write func, close func
chanSink :: MonadIO m => chan -> (chan -> a -> IO ()) -> (chan -> IO ()) -> Sink a m ()
chanSink ch writer closer = do
    CL.mapM_ $ liftIO . writer ch
    liftIO $ closer ch
