{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Conduit
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
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
    , padRight
    , padBottom
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
import Control.Monad.State
import Control.Monad.STM (atomically, STM)
import Data.Char (isLetter, isSpace)
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.TQueue
import Data.Conduit.TMChan hiding ((<=>))
import Data.Default
import Data.Word (Word8)
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid ((<>), appEndo, Endo(..))
import Data.String
import Data.Text (singleton)
import Data.Text.Markup ((@@), Markup)
import GHC.IO.Handle
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Lens.Micro.TH (makeLenses)
import Network (withSocketsDo)
import Parchment.Parsing
import Text.Parsec

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
instance Show Fchar where
    show Fchar {_ch = ch, _attr = attr} = show ch
data Sess = Sess {
    _scrollback :: [[Fchar]]
    , _input :: String
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (Sess -> EventM () (Next Sess))
    , _send_queue :: TQueue BS.ByteString
    , _telnet_state :: ParseState BS.ByteString
    , _esc_seq_state :: ParseState BS.ByteString
    , _char_attr :: V.Attr
    }
data RecvEvent = RecvEvent BS.ByteString
makeLenses ''Fchar
makeLenses ''Sess

drawScrollback :: [[Fchar]] -> Int -> Widget()
drawScrollback lines num = foldr (<=>) (str "") $ map drawScrollbackLine $ take num lines
    where drawScrollbackLine [] = str " " -- handle blank case
          drawScrollbackLine s = markup . mconcat . map fcharToMarkup $ s
          fcharToMarkup = \t -> (singleton $ _ch t) @@ (_attr t)

drawUI :: Sess -> [Widget()]
drawUI (Sess {_scrollback = sb, _input = input, _cursor = cursor}) =
    [vBox [ drawScrollback sb 10
          , padTop Max $ hBorder
          , showCursor () (Location (cursor, 0))
              (if length input > 0 then str input else str " ")
          ]]

createBindings :: [(V.Event, (Sess -> EventM () (Next Sess)))] ->
    Map.Map V.Event (Sess -> EventM () (Next Sess))
createBindings = (mconcat . map createBinding)
    where createBinding (e, b) = Map.insert e b Map.empty

handleEvent :: Sess -> BrickEvent () RecvEvent -> EventM () (Next Sess)
handleEvent st (VtyEvent e) =
    case Map.lookup e (_bindings st) of
        Just b -> b st
        Nothing -> continue st
handleEvent st (AppEvent e) =
    case e of
        RecvEvent bs -> continue $ handleServerData st bs
handleEvent st _ = continue st

recvData :: Sess -> BS.ByteString -> Sess
recvData st bs = st & scrollback .~ ((_scrollback st) ++ [testText $ show $ length $
    ((map testText) . (splitOn "\n") . filter
        (or . \x -> map ($ x) [isLetter, isSpace]) $ BSC.unpack bs)])

word8ToChar :: Word8 -> Char
word8ToChar = BSC.head . BS.singleton

testText :: String -> [Fchar]
testText = map testChar

testChar :: Char -> Fchar
testChar c = Fchar {
    _ch = c,
    _attr = V.Attr {
        V.attrStyle = V.Default,
        V.attrForeColor = V.SetTo V.blue,
        V.attrBackColor = V.Default
        }
    }

addKey :: Sess -> Char -> Sess
addKey st k = (st & input .~ ((_input st) ++ [k])) & cursor .~ ((_cursor st) + 1)

delKey :: Sess -> Sess
delKey st =
    if length (_input st) == 0 then
        st
    else
        (st & input .~ (init (_input st))) & cursor .~ ((_cursor st) - 1)

sendInput :: Sess -> IO Sess
sendInput st = do
    atomically $ writeTQueue (_send_queue st) (BSC.pack $ (_input st) ++ "\r\n")
    return st

clearInput :: Sess -> Sess
clearInput st = (st & input .~ "") & cursor .~ 0

rawKeyBinding :: Char -> (V.Event, (Sess -> EventM () (Next Sess)))
rawKeyBinding c = ((V.EvKey (V.KChar c) []), \st -> continue $ addKey st c)

rawKeys :: String
rawKeys = "abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()_+-=_+[]\\;',./{}|:\"<>? `~"

app :: App Sess RecvEvent ()
app =
    App { appDraw = drawUI
        , appHandleEvent = handleEvent
        , appAttrMap = const $ attrMap V.defAttr []
        , appStartEvent = return
        , appChooseCursor = showFirstCursor
        }

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
            ] ++ map rawKeyBinding rawKeys)
        , _send_queue = q
        , _telnet_state = NotInProgress
        , _esc_seq_state = NotInProgress
        , _char_attr = V.defAttr
        }

-- channel, write func, close func
chanSink :: MonadIO m => chan -> (chan -> a -> IO ()) -> (chan -> IO ()) -> Sink a m ()
chanSink ch writer closer = do
    CL.mapM_ $ liftIO . writer ch
    liftIO $ closer ch

chanWriteRecvEvent :: Chan RecvEvent -> BS.ByteString -> IO ()
chanWriteRecvEvent c s = writeChan c (RecvEvent s)

addScrollbackChar :: [[Fchar]] -> Fchar -> [[Fchar]]
addScrollbackChar sb c =
    if (_ch c) == '\n' then
        -- Add new empty line.
        sb ++ [[]]
    else if (_ch c) == '\r' then
        sb
    else if length sb == 0 then
        -- Init with char.
        [[c]]
    else
        -- Add char to end of last line.
        init sb ++ [(last sb ++ [c])]

updateCharAttr :: V.Attr -> BS.ByteString -> V.Attr
updateCharAttr attr seq =
    case parse escSeqParser "error" (BSC.unpack seq) of
         Right [] -> V.defAttr -- handle \ESC[m case
         Right parts -> foldFuncList (map escSeqPartTransform parts) attr
         Left _ -> attr

foldFuncList :: [a -> a] -> a -> a
foldFuncList = appEndo . foldMap Endo . reverse

escSeqPartTransform :: String -> V.Attr -> V.Attr
escSeqPartTransform "0" = const V.defAttr
escSeqPartTransform "1" = flip V.withStyle V.bold
escSeqPartTransform "30" = flip V.withForeColor V.black
escSeqPartTransform "31" = flip V.withForeColor V.red
escSeqPartTransform "32" = flip V.withForeColor V.green
escSeqPartTransform "33" = flip V.withForeColor V.yellow
escSeqPartTransform "34" = flip V.withForeColor V.blue
escSeqPartTransform "35" = flip V.withForeColor V.magenta
escSeqPartTransform "36" = flip V.withForeColor V.cyan
escSeqPartTransform "37" = flip V.withForeColor V.white
--escSeqPartTransform "39" = \a ->
--    let curr_style = (V.attrStyle a)
--        curr_back = (V.attrBackColor a)
--        in V.withBackColor $ (V.withStyle V.defAttr curr_style) curr_back
escSeqPartTransform "40" = flip V.withBackColor V.black
escSeqPartTransform "41" = flip V.withBackColor V.red
escSeqPartTransform "42" = flip V.withBackColor V.green
escSeqPartTransform "43" = flip V.withBackColor V.yellow
escSeqPartTransform "44" = flip V.withBackColor V.blue
escSeqPartTransform "45" = flip V.withBackColor V.magenta
escSeqPartTransform "46" = flip V.withBackColor V.cyan
escSeqPartTransform "47" = flip V.withBackColor V.white
--escSeqPartTransform "49" = \a ->
--    let curr_style = (V.attrStyle a)
--        curr_fore = (V.attrForeColor a)
--        in V.withForeColor $ (V.withStyle V.defAttr curr_style) curr_fore
escSeqPartTransform _ = id -- ignore unknown parts

-- Returns the internal parts of the esc sequence.
escSeqParser :: Parsec String () [String]
escSeqParser = string "\ESC[" *> sepBy (many1 digit) (char ';') <* char 'm'

handleServerByte :: Sess -> Word8 -> Sess
handleServerByte st b =
    execState (
        do
            sess <- get
            let new_telnet = parseTelnet (_telnet_state sess) b
            put (sess & telnet_state .~ new_telnet)
            case new_telnet of
                NotInProgress -> do
                    sess <- get
                    let new_esc_seq = parseEscSeq (_esc_seq_state sess) b
                    put (sess & esc_seq_state .~ new_esc_seq)
                    case new_esc_seq of
                         NotInProgress -> do
                             sess <- get
                             put (sess & scrollback .~ addScrollbackChar
                                 (_scrollback st) (Fchar {
                                     _ch =  word8ToChar b, _attr = (_char_attr sess)}))
                             return ()
                         Success seq -> do
                             sess <- get
                             put (sess & char_attr .~ updateCharAttr ((_char_attr sess)) seq)
                             return ()
                         _ -> return ()
                    return ()
                _ -> return ()
        ) st

handleServerData :: Sess -> BS.ByteString -> Sess
handleServerData st bs = foldl' handleServerByte st $ BS.unpack bs

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
