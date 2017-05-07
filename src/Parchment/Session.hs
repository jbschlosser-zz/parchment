{-# LANGUAGE TemplateHaskell #-}

module Parchment.Session
    ( Sess(..)
    , initialSession
    , addKey
    , delKey
    , sendToServer
    , sendRawToServer
    , clearInputLine
    , writeBuffer
    , writeBufferLn
    , bind
    , getInput
    , addToHistory
    , receiveServerData
    , pageUp
    , pageDown
    , scrollLines
    , historyOlder
    , historyNewer
    , historyNewest
    , scrollHistory
    , highlightStr
    , unhighlightStr
    , searchBackwards
    -- lenses
    , buffer
    , buf_lines
    , cursor
    , scroll_loc
    , telnet_cmds
    , scm_env
    , bindings
    ) where

import Brick.Types (EventM, Next)
import Brick.Util (clamp)
import Control.Concurrent.STM.TQueue
import Control.Monad.State (get, put, execState)
import Control.Monad.STM (atomically)
import Data.Array ((!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl')
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as S
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Language.Scheme.Types hiding (bindings)
import Lens.Micro ((.~), (^.), (&), (%~), ix)
import Lens.Micro.TH (makeLenses)
import Parchment.EscSeq
import Parchment.FString
import Parchment.ParseState
import qualified Parchment.RingBuffer as RB
import Parchment.Telnet
import Text.Parsec hiding (Error, getInput)
import Text.Regex.TDFA (CompOption(..), ExecOption(..))
import qualified Text.Regex.TDFA.String as R

data Sess = Sess
    { _buffer :: RB.RingBuffer FString
    , _buf_lines :: Int
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (Sess -> EventM () (Next Sess))
    , _send_queue :: TQueue BS.ByteString
    , _telnet_state :: ParseState BS.ByteString
    , _esc_seq_state :: ParseState BS.ByteString
    , _char_attr :: V.Attr
    , _scroll_loc :: Int
    , _history :: [String]
    , _history_loc :: Int
    , _scm_env :: Env
    , _last_search :: Maybe SearchResult -- str, line, start index, end index
    , _telnet_cmds :: [BS.ByteString]
    }
data SearchResult = SearchResult
    { _search :: String
    , _line :: Int
    , _start :: Int
    , _end :: Int
    }
searchResult :: String -> Int -> Int -> Int -> SearchResult
searchResult search line start end = SearchResult
    { _search = search
    , _line = line
    , _start = start
    , _end = end
    }
makeLenses ''Sess
makeLenses ''SearchResult

-- Initial state of the session data.
initialSession :: TQueue BS.ByteString ->
    Map.Map V.Event (Sess -> EventM () (Next Sess)) -> Env -> Sess
initialSession q bindings scm_env = Sess
    { _buffer = flip RB.push emptyF $ RB.newInit emptyF 50000 -- lines in buffer
    , _buf_lines = 0
    , _cursor = 0
    , _bindings = bindings
    , _send_queue = q
    , _telnet_state = NotInProgress
    , _esc_seq_state = NotInProgress
    , _char_attr = V.defAttr
    , _scroll_loc = 0
    , _history = [""]
    , _history_loc = 0
    , _scm_env = scm_env
    , _last_search = Nothing
    , _telnet_cmds = []
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

bind :: V.Event -> (Sess -> EventM () (Next Sess)) -> Sess -> Sess
bind event action sess = sess & bindings %~ (\b -> Map.insert event action b)

getInput :: Sess -> String
getInput sess = flip (^.) (history . ix (sess ^. history_loc)) $ sess

clearInputLine :: Sess -> Sess
clearInputLine sess = sess &
    history . ix (sess ^. history_loc) .~ "" &
    cursor .~ 0

writeBuffer :: FString -> Sess -> Sess
writeBuffer str sess = foldl' addBufferChar sess str

writeBufferLn :: FString -> Sess -> Sess
writeBufferLn str = writeBuffer (str ++ [FChar { _ch = '\n', _attr = V.defAttr}])

-- Highlight line, start index, end index.
highlightStr :: (Int, Int, Int) -> Sess -> Sess
highlightStr = flip modifyBuffer $ withStyle V.standout

-- Unhighlight line, start index, end index.
unhighlightStr :: (Int, Int, Int) -> Sess -> Sess
unhighlightStr = flip modifyBuffer $ withStyle V.defaultStyleMask

searchBackwards :: String -> Sess -> Sess
searchBackwards str sess =
    case R.compile regexCompOpt regexExecOpt str of
         Left err -> flip writeBufferLn sess . colorize V.red $ "Regex error: " ++ err
         Right regex -> case searchBackwardsHelper regex (sess ^. buffer) (startLine sess) of
                             Just sr@(line,_,_) -> highlightStr sr . setSearchRes str (Just sr) .
                                 unhighlightPrevious . scrollLines
                                     (line - (sess ^. scroll_loc)) $ sess
                             Nothing -> writeBufferLn (colorize V.red $ "Not found!") .
                                 setSearchRes str Nothing . unhighlightPrevious $ sess
    where startLine sess =
              case sess ^. last_search of
                   Nothing -> 0
                   Just sr -> if (sr ^. search) == str then (sr ^. line) + 1 else 0
          unhighlightPrevious sess =
              case sess ^. last_search of
                   Nothing -> sess
                   Just sr ->
                       unhighlightStr ((sr ^. line), (sr ^. start), (sr ^. end)) $ sess
          setSearchRes str (Just (line, start, end)) sess =
              sess & last_search .~ Just (searchResult str line start end)
          setSearchRes _ Nothing sess = sess & last_search .~ Nothing

scrollLines :: Int -> Sess -> Sess
scrollLines n sess = sess & scroll_loc %~
    (\sl -> clamp 0 (RB.length (sess ^. buffer) - (sess ^. buf_lines)) $ sl + n)

pageUp :: Sess -> Sess
pageUp = scrollLines 10

pageDown :: Sess -> Sess
pageDown = scrollLines $ -10

addToHistory :: String -> Sess -> Sess
addToHistory s sess = sess &
    history %~ (++ [s, ""]) . init &
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

historyNewest :: Sess -> Sess
historyNewest sess = scrollHistory (length $ sess ^. history) sess

sendToServer :: String -> Sess -> IO Sess
sendToServer str sess = do
    atomically $ writeTQueue (sess ^. send_queue) (BSC.pack $ str ++ "\r\n")
    return sess

sendRawToServer :: [Word8] -> Sess -> IO Sess
sendRawToServer bytes sess = do
    atomically $ writeTQueue (sess ^. send_queue) $ BS.pack bytes
    return sess

receiveServerData :: Sess -> BS.ByteString -> Sess
receiveServerData sess bs = foldl' handleServerByte sess $ BS.unpack bs

-- === HELPER FUNCTIONS ===
-- (line, start index, end index), modification func, session
modifyBuffer :: (Int, Int, Int) -> (FChar -> FChar) -> Sess -> Sess
modifyBuffer (line, start, end) func sess =
    sess & buffer %~ \buf -> RB.update buf line new_str
    where line_str = (sess ^. buffer) RB.! line
          replaceAtIndex f n ls = a ++ ((f item):b)
              where (a, (item:b)) = splitAt n ls
          new_str = (foldr (flip (.)) id $ flip map [start..end] $
                    replaceAtIndex func) line_str

regexCompOpt :: CompOption
regexCompOpt = CompOption
    { caseSensitive = True
    , multiline = False
    , rightAssoc = True
    , newSyntax = True
    , lastStarGreedy = False
    }

regexExecOpt :: ExecOption
regexExecOpt = ExecOption {captureGroups = True}

-- Input: Search string, buffer, starting line.
-- Returns: Line, start index, end index if found; Nothing otherwise.
searchBackwardsHelper :: R.Regex -> RB.RingBuffer FString -> Int -> Maybe (Int, Int, Int)
searchBackwardsHelper r buf start_line =
    case S.findIndexL isJust search_results of
         Nothing -> Nothing
         Just idx -> Just (start_line + idx, start, start + len - 1)
             where (start, len) = fromJust (S.index search_results idx)
    where search_results = fmap (findInFString r) . RB.drop start_line $ buf

-- Returns (start, length) if found; Nothing otherwise.
findInFString :: R.Regex -> FString -> Maybe (Int, Int)
findInFString r fs =
    case R.execute r $ removeFormatting fs of
         Left _ -> Nothing
         Right Nothing -> Nothing
         Right (Just ma) -> Just $ ma ! 0

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
                             put (addBufferChar sess
                                 (FChar { _ch = BSC.head . BS.singleton $ b
                                        , _attr = (_char_attr sess)}))
                             return ()
                         Success seq -> do
                             sess <- get
                             put (sess & char_attr .~ updateCharAttr ((_char_attr sess)) seq)
                             return ()
                         _ -> return ()
                    return ()
                Success t -> do
                    put (sess & telnet_cmds %~ flip (++) [t]
                         & telnet_state .~ NotInProgress)
                    return ()
                _ -> return ()
        ) sess

addBufferChar :: Sess -> FChar -> Sess
addBufferChar sess c =
    if (_ch c) == '\n' then
        sess & buffer %~ (\buf -> RB.push buf emptyF)
             & last_search %~ \ls ->
                case ls of
                    Just res ->
                        if (res ^. line) + 1 >= RB.length (sess ^. buffer) then
                            Nothing
                        else
                            Just $ res & line %~ (+1)
                    Nothing -> Nothing
    else if (_ch c) == '\r' then
        sess
    else
        -- Add char to end of last line.
        sess & buffer %~ \buf -> RB.update buf 0 ((buf RB.! 0) ++ [c])

clampExclusive :: Int -> Int -> Int -> Int
clampExclusive min max = clamp min (max - 1)

updateCharAttr :: V.Attr -> BS.ByteString -> V.Attr
updateCharAttr attr seq =
    case parse escSeqPartParser "error" (BSC.unpack seq) of
         Right [] -> V.defAttr -- handle \ESC[m case
         Right parts -> foldr (flip (.)) id (map escSeqPartTransform parts) attr
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
