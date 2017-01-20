{-# LANGUAGE TemplateHaskell #-}

module Parchment.Session
    ( Fchar(..)
    , Sess(..)
    , initialSession
    , addKey
    , delKey
    , sendToServer
    , clearInputLine
    , writeScrollback
    , writeScrollbackLn
    , colorize
    , defaultColor
    , formatStr
    , getInput
    , nextHistory
    , receiveServerData
    , pageUp
    , pageDown
    , scrollLines
    , historyOlder
    , historyNewer
    , scrollHistory
    , foldFuncList
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
import qualified Graphics.Vty as V
import Language.Scheme.Core
import Language.Scheme.Types
import Lens.Micro ((.~), (^.), (&), (%~), over, ix)
import Lens.Micro.TH (makeLenses)
import Parchment.Parsing
import Text.Parsec hiding (Error, getInput)

data Fchar = Fchar {_ch :: Char, _attr :: V.Attr}
instance Show Fchar where
    show Fchar {_ch = ch, _attr = attr} = show ch
makeLenses ''Fchar

data Sess = Sess {
    _scrollback :: [[Fchar]]
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (Sess -> EventM () (Next Sess))
    , _send_queue :: TQueue BS.ByteString
    , _telnet_state :: ParseState BS.ByteString
    , _esc_seq_state :: ParseState BS.ByteString
    , _char_attr :: V.Attr
    , _scroll_loc :: Int
    , _scroll_limit :: Int
    , _history :: [String]
    , _history_loc :: Int
    , _scm_env :: Env
    }
makeLenses ''Sess

-- Initial state of the session data.
initialSession :: TQueue BS.ByteString ->
    Map.Map V.Event (Sess -> EventM () (Next Sess)) -> Env -> Sess
initialSession q bindings scm_env =
    Sess {
        _scrollback = [[]]
        , _cursor = 0
        , _bindings = bindings
        , _send_queue = q
        , _telnet_state = NotInProgress
        , _esc_seq_state = NotInProgress
        , _char_attr = V.defAttr
        , _scroll_loc = 0
        , _scroll_limit = 10000 -- lines in scrollback buffer
        , _history = [""]
        , _history_loc = 0
        , _scm_env = scm_env
        }

-- === ACTIONS ===
addKey :: Char -> Sess -> Sess
addKey k sess = sess & history . ix (sess ^. history_loc) %~ (++ [k]) & cursor %~ (+1)

delKey :: Sess -> Sess
delKey sess =
    if length (getInput sess) == 0 then
        sess
    else
        sess & history . ix (sess ^. history_loc) %~ init & cursor %~ subtract 1

getInput :: Sess -> String
getInput sess = flip (^.) (history . ix (sess ^. history_loc)) $ sess

clearInputLine :: Sess -> Sess
clearInputLine sess = sess & history . ix (sess ^. history_loc) .~ "" & cursor .~ 0

writeScrollback :: [Fchar] -> Sess -> Sess
writeScrollback str sess = sess & scrollback .~
    foldl' (addScrollbackChar $ sess ^. scroll_limit) (sess ^. scrollback) str

writeScrollbackLn :: [Fchar] -> Sess -> Sess
writeScrollbackLn str = writeScrollback (str ++ [Fchar { _ch = '\n', _attr = V.defAttr}])

colorize :: V.Color -> String -> [Fchar]
colorize color str = map (\c ->
                          Fchar {_ch = c, _attr = V.withForeColor V.defAttr color}) str 

applyAttr :: V.Attr -> String -> [Fchar]
applyAttr a str = map (\c -> Fchar {_ch = c, _attr = a}) str 

defaultColor :: String -> [Fchar]
defaultColor str = map (\c -> Fchar {_ch = c, _attr = V.defAttr}) str 

formatStr :: String -> [Fchar]
formatStr s = case runParser formatStrParser V.defAttr "source" s of
                   -- TODO: Maybe better error handling?
                   Left err -> defaultColor $ s
                   Right fc -> foldr (++) [] fc

-- Matches a format part.
formatPartParser :: Parsec String V.Attr String
formatPartParser = do
    char '{'
    code <- oneOf "xbrgyumcwH{"
    attr <- case code of
                   'x' -> return V.defAttr
                   'b' -> return $ V.withForeColor V.defAttr V.black
                   'r' -> return $ V.withForeColor V.defAttr V.red
                   'g' -> return $ V.withForeColor V.defAttr V.green
                   'y' -> return $ V.withForeColor V.defAttr V.yellow
                   'u' -> return $ V.withForeColor V.defAttr V.blue
                   'm' -> return $ V.withForeColor V.defAttr V.magenta
                   'c' -> return $ V.withForeColor V.defAttr V.cyan
                   'w' -> return $ V.withForeColor V.defAttr V.white
                   _ -> getState
    putState attr
    let chars = case code of
                     '{' -> "{"
                     _ -> ""
    return chars

-- Matches a whole string with optional format parts.
formatStrParser :: Parsec String V.Attr [[Fchar]]
formatStrParser = many $ do
    part <- formatPartParser <|> many1 (noneOf "{")
    state <- getState
    return $ applyAttr state part

scrollLines :: Int -> Sess -> Sess
scrollLines n sess = sess & scroll_loc %~
    (\sl -> clampExclusive 0 (length $ sess ^. scrollback) $ sl + n)

pageUp :: Sess -> Sess
pageUp = scrollLines 10

pageDown :: Sess -> Sess
pageDown = scrollLines $ -10

nextHistory :: Sess -> Sess
nextHistory sess = sess &
    history %~ (++ [""]) &
    history_loc .~ (length $ sess ^. history) & -- based on old history length
    cursor .~ 0

scrollHistory :: Int -> Sess -> Sess
scrollHistory n sess = sess &
    history_loc .~ new_loc &
    cursor .~ new_cursor
    where new_loc = clampExclusive 0 (length $ sess ^. history) $ (sess ^. history_loc) + n
          new_cursor = length $ sess ^. (history . ix new_loc)

historyOlder :: Sess -> Sess
historyOlder = scrollHistory $ -1

historyNewer :: Sess -> Sess
historyNewer = scrollHistory 1

sendToServer :: String -> Sess -> IO Sess
sendToServer str sess = do
    atomically $ writeTQueue (_send_queue sess) (BSC.pack $ str ++ "\r\n")
    return sess

receiveServerData :: Sess -> BS.ByteString -> Sess
receiveServerData sess bs = foldl' handleServerByte sess $ BS.unpack bs

-- === HELPER FUNCTIONS ===
handleServerByte :: Sess -> Word8 -> Sess
handleServerByte sess b =
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
                                 (_scroll_limit sess) (_scrollback sess) (Fchar
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
        ) sess

addScrollbackChar :: Int -> [[Fchar]] -> Fchar -> [[Fchar]]
addScrollbackChar limit sb c =
    if (_ch c) == '\n' then
        -- Add new empty line.
        if length sb >= limit then
            (tail sb) ++ [[]]
        else
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

clampExclusive :: Int -> Int -> Int -> Int
clampExclusive min max = clamp min (max - 1)

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
