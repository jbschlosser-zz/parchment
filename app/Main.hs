{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Map.Lazy as Map
import qualified Graphics.Vty as V
import Brick.AttrMap (attrMap, AttrMap)
import Brick.Main
    ( App(..)
    , defaultMain
    , customMain
    , resizeOrQuit
    , neverShowCursor
    , showFirstCursor
    , halt
    , continue
    , suspendAndResume
    )
import Brick.Markup (markup, (@?))
import Brick.Types
    ( Widget
    , Padding(..)
    , Location(..)
    , Next(..)
    , EventM
    , BrickEvent(..)
    )
import Brick.Util (on, fg)
import Brick.Widgets.Core
    ( (<=>)
    , (<+>)
    , padLeft
    , padTop
    , str
    , vBox
    , hBox
    , showCursor
    )
import Brick.Widgets.Border (vBorder, hBorder)
import Control.Concurrent (forkIO, newChan, Chan, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM.TQueue
import Control.Monad (void, forever, liftM)
import Control.Monad.IO.Class
import Control.Monad.STM (atomically, STM)
import Data.Char (isAscii)
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.TQueue
import Data.Conduit.TMChan hiding ((<=>))
import Data.Default
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.String
import Data.Text (singleton)
import Data.Text.Markup ((@@), Markup)
import GHC.IO.Handle
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Lens.Micro.TH (makeLenses)
import Network (withSocketsDo)
import System.IO.Streams (
    inputStreamToHandle
    , outputStreamToHandle
    , makeInputStream
    , makeOutputStream
    , lockingInputStream
    , lockingOutputStream
    , fromByteString
    )

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
data St = St {
    _scrollback :: [[Fchar]]
    , _input :: String
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (St -> EventM () (Next St))
    , _send_queue :: TQueue BS.ByteString
    }
data RecvEvent = RecvEvent BS.ByteString
makeLenses ''Fchar
makeLenses ''St

drawScrollback :: [[Fchar]] -> Int -> Widget()
drawScrollback lines num = foldr (<=>) (str "") $ map drawScrollbackLine $ take num lines
    where drawScrollbackLine = markup . mconcat . map fcharToMarkup
          fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)

drawUI :: St -> [Widget()]
drawUI (St {_scrollback = sb, _input = input, _cursor = cursor}) =
    [vBox [ drawScrollback sb 10
          , padTop Max $ hBorder
          , showCursor () (Location (cursor, 0))
              (if length input > 0 then str input else str " ")
          ]]

createBindings :: [(V.Event, (St -> EventM () (Next St)))] ->
    Map.Map V.Event (St -> EventM () (Next St))
createBindings = (mconcat . map createBinding)
    where createBinding (e, b) = Map.insert e b Map.empty

handleEvent :: St -> BrickEvent () RecvEvent -> EventM () (Next St)
handleEvent st (VtyEvent e) =
    case Map.lookup e (_bindings st) of
        Just b -> b st
        Nothing -> continue st
handleEvent st (AppEvent e) =
    case e of
        RecvEvent bs -> continue $ recvData st bs
handleEvent st _ = continue st

recvData :: St -> BS.ByteString -> St
recvData st bs = st & scrollback .~ ((_scrollback st) ++
    ((map testText) . (splitOn "\n") . filter isAscii $ BS.unpack bs))

testText :: String -> [Fchar]
testText = map (\c -> Fchar {
    _ch = c,
    _attr = V.Attr {
        V.attrStyle = V.Default,
        V.attrForeColor = V.SetTo V.blue,
        V.attrBackColor = V.Default
        }
    })

addKey :: St -> Char -> St
addKey st k = (st & input .~ ((_input st) ++ [k])) & cursor .~ ((_cursor st) + 1)

delKey :: St -> St
delKey st =
    if length (_input st) == 0 then
        st
    else
        (st & input .~ (init (_input st))) & cursor .~ ((_cursor st) - 1)

sendInput :: St -> IO St
sendInput st = do
    atomically $ writeTQueue (_send_queue st) (BS.pack $ (_input st) ++ "\r\n")
    return st

clearInput :: St -> St
clearInput st = (st & input .~ "") & cursor .~ 0

rawKeyBinding :: Char -> (V.Event, (St -> EventM () (Next St)))
rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey st c)

rawKeys :: String
rawKeys = "abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"

app :: App St RecvEvent ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        }

initialState :: TQueue BS.ByteString -> St
initialState q =
    St {
        _scrollback = [[]]
        , _input = ""
        , _cursor = 0
        , _bindings = createBindings
            ([ ((V.EvKey V.KEsc []), \st -> halt st)
            , ((V.EvKey V.KBS []), \st -> continue $ delKey st)
            , ((V.EvKey V.KEnter []), \st -> do
                s <- liftIO $ sendInput st
                continue $ clearInput s)
            ] ++ map rawKeyBinding rawKeys)
        , _send_queue = q
        }

-- channel, write func, close func
chanSink :: MonadIO m => chan -> (chan -> a -> IO ()) -> (chan -> IO ()) -> Sink a m ()
chanSink ch writer closer = do
    CL.mapM_ $ liftIO . writer ch
    liftIO $ closer ch

chanWriteRecvEvent :: Chan RecvEvent -> BS.ByteString -> IO ()
chanWriteRecvEvent c s = writeChan c (RecvEvent s)

serverDataReceived :: MonadIO m => TQueue a -> Chan RecvEvent -> Sink a m ()
serverDataReceived q c = sinkTQueue q

main :: IO ()
main = withSocketsDo $ void $ do
    send_queue <- newTQueueIO
    recv_queue <- newTQueueIO
    eventChan <- newChan
    tid <- forkIO $ runTCPClient (clientSettings 4000 (BS.pack "127.0.0.1")) $ \server ->
        void $ concurrently
            (appSource server $$ chanSink eventChan chanWriteRecvEvent (\c -> return ()))
            (sourceTQueue send_queue $$ appSink server)
    customMain (V.mkVty def) (Just eventChan) app (initialState send_queue)
