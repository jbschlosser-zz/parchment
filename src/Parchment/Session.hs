{-# LANGUAGE TemplateHaskell #-}

module Parchment.Session
    ( Fchar(..)
    , Sess(..)
    , addKey
    , delKey
    , sendInput
    , clearInput
    , receiveServerData
    , pageUp
    , pageDown
    ) where

import Brick.Types (EventM, Next)
import Brick.Util (clamp)
import Control.Concurrent.STM.TQueue
import Control.Monad.State (get, put, execState)
import Control.Monad.STM (atomically, STM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import qualified Data.Map.Lazy as Map
import Data.Monoid (appEndo, Endo(..))
import Data.Word (Word8)
import Graphics.Vty as V
import Lens.Micro ((.~), (^.), (&), (%~), over)
import Lens.Micro.TH (makeLenses)
import Parchment.Parsing
import Text.Parsec (parse)

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
instance Show Fchar where
    show Fchar {_ch = ch, _attr = attr} = show ch
makeLenses ''Fchar

data Sess = Sess {
    _scrollback :: [[Fchar]]
    , _input :: String
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (Sess -> EventM () (Next Sess))
    , _send_queue :: TQueue BS.ByteString
    , _telnet_state :: ParseState BS.ByteString
    , _esc_seq_state :: ParseState BS.ByteString
    , _char_attr :: V.Attr
    , _scroll_loc :: Int
    }
makeLenses ''Sess

-- === ACTIONS ===
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

receiveServerData :: Sess -> BS.ByteString -> Sess
receiveServerData st bs = foldl' handleServerByte st $ BS.unpack bs

scrollLines :: Sess -> Int -> Sess
scrollLines st n = (st & scroll_loc .~ (clamp 0 (length $ _scrollback st) $
    (_scroll_loc st) + n))

pageUp :: Sess -> Sess
pageUp = flip scrollLines 10

pageDown :: Sess -> Sess
pageDown = flip scrollLines $ -10

-- === HELPER FUNCTIONS ===
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
                                 (_scrollback st) (Fchar
                                     { _ch = BSC.head . BS.singleton $ b
                                     , _attr = (_char_attr sess)}))
                             return ()
                         Success seq -> do
                             sess <- get
                             put (sess & char_attr .~ updateCharAttr ((_char_attr sess)) seq)
                             return ()
                         _ -> return ()
                    return ()
                _ -> return ()
        ) st

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

foldFuncList :: [a -> a] -> a -> a
foldFuncList = appEndo . foldMap Endo . reverse

updateCharAttr :: V.Attr -> BS.ByteString -> V.Attr
updateCharAttr attr seq =
    case parse escSeqPartParser "error" (BSC.unpack seq) of
         Right [] -> V.defAttr -- handle \ESC[m case
         Right parts -> foldFuncList (map escSeqPartTransform parts) attr
         Left _ -> attr

escSeqPartTransform :: String -> V.Attr -> V.Attr
escSeqPartTransform s =
    case s of
        "0" -> const V.defAttr
        "1" -> flip V.withStyle V.bold
        "30" -> flip V.withForeColor V.black
        "31" -> flip V.withForeColor V.red
        "32" -> flip V.withForeColor V.green
        "33" -> flip V.withForeColor V.yellow
        "34" -> flip V.withForeColor V.blue
        "35" -> flip V.withForeColor V.magenta
        "36" -> flip V.withForeColor V.cyan
        "37" -> flip V.withForeColor V.white
        "39" -> \a -> V.Attr
            { V.attrStyle = (V.attrStyle a)
            , V.attrForeColor = V.Default
            , V.attrBackColor = (V.attrBackColor a)
            }
        "40" -> flip V.withBackColor V.black
        "41" -> flip V.withBackColor V.red
        "42" -> flip V.withBackColor V.green
        "43" -> flip V.withBackColor V.yellow
        "44" -> flip V.withBackColor V.blue
        "45" -> flip V.withBackColor V.magenta
        "46" -> flip V.withBackColor V.cyan
        "47" -> flip V.withBackColor V.white
        "49" -> \a -> V.Attr
            { V.attrStyle = (V.attrStyle a)
            , V.attrForeColor = (V.attrForeColor a)
            , V.attrBackColor = V.Default
            }
        _ -> id
