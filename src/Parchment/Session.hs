{-# LANGUAGE TemplateHaskell #-}

module Parchment.Session
    ( Sess(..)
    , Settings(..)
    , initialSession
    , defaultSettings
    , addInput
    , backspaceInput
    , deleteInput
    , sendToServer
    , sendRawToServer
    , sendGmcp
    , clearInput
    , moveCursor
    , writeBuffer
    , writeBufferLn
    , switchToBuffer
    , toggleBuffer
    , logError
    , logInfo
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
    , debugBufferNum
    , mainBufferNum
    , currentBufferNum
    -- lenses
    , settings
    , hostname
    , port
    , buffers
    , buffer
    , cursor
    , scm_env
    , bindings
    , recv_state
    , telnet_cmds
    , text
    , currentBuffer
    ) where

import Brick.Util (clamp)
import Brick.Types (EventM, Next)
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)
import Data.Array ((!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl', splitAt)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as S
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Language.Scheme.Types hiding (bindings)
import Lens.Micro ((.~), (^.), (&), (%~), (^?), ix)
import Lens.Micro.TH (makeLenses)
import Parchment.EscSeq
import qualified Parchment.Indexed as I
import Parchment.FString
import Parchment.ParseState
import qualified Parchment.RingBuffer as RB
import Parchment.Telnet
import Parchment.Util
import qualified Text.Regex.TDFA.String as R

data Sess = Sess
    { _settings :: Settings
    , _buffers :: I.Indexed [Buffer]
    , _history :: I.Indexed (RB.RingBuffer String)
    , _cursor :: Int
    , _bindings :: Map.Map V.Event (Sess -> EventM () (Next Sess))
    , _recv_state :: RecvState
    , _send_queue :: TQueue BS.ByteString
    , _scm_env :: Env
    }
data Buffer = Buffer
    { _buffer :: I.Indexed (RB.RingBuffer FString)
    , _last_search :: Maybe SearchResult
    }
emptyBuffer :: Int -> Buffer
emptyBuffer nlines = Buffer
    { _buffer = I.indexed (RB.newInit emptyF nlines & RB.push emptyF) $ const (0,1)
    , _last_search = Nothing
    }
data Settings = Settings
    { _hostname :: String
    , _port :: Int
    }
defaultSettings :: String -> Int -> Settings
defaultSettings host port = Settings
    { _hostname = host
    , _port = port
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
data RecvState = RecvState
    { _text :: FString
    , _telnet_state :: ParseState BS.ByteString
    , _telnet_cmds :: [BS.ByteString]
    , _esc_seq_state :: ParseState BS.ByteString
    , _char_attr :: V.Attr
    }
blankRecvState :: RecvState
blankRecvState = RecvState
    { _text = []
    , _telnet_state = NotInProgress
    , _telnet_cmds = []
    , _esc_seq_state = NotInProgress
    , _char_attr = V.defAttr
    }
makeLenses ''Sess
makeLenses ''Buffer
makeLenses ''Settings
makeLenses ''SearchResult
makeLenses ''RecvState

-- Initial state of the session data.
initialSession :: Settings ->
    TQueue BS.ByteString ->
    Map.Map V.Event (Sess -> EventM () (Next Sess)) ->
    Env ->
    Sess
initialSession settings q bindings scm_env = Sess
    { _settings = settings
    , _buffers = I.indexed [emptyBuffer b_lines, emptyBuffer b_lines] (\b -> (0, length b))
          & I.index .~ mainBufferNum
    , _history = I.indexed (RB.newInit "" 1000 & RB.push "") (\h -> (0, RB.length h))
    , _cursor = 0
    , _bindings = bindings
    , _recv_state = blankRecvState
    , _send_queue = q
    , _scm_env = scm_env
    }
    where b_lines = 50000

-- === ACTIONS ===
getInput :: Sess -> String
getInput sess = sess ^. history & I.atCurrIndex (RB.!)

addInput :: Char -> Sess -> Sess
addInput ch sess = sess & history . I.value %~ RB.update hist_index (left ++ ch:right)
                        & moveCursor 1
    where hist_index = sess ^. history . I.index
          input = getInput sess
          (left, right) = splitAt (sess ^. cursor) input

backspaceInput :: Sess -> Sess
backspaceInput sess
    | left == "" = sess
    | otherwise = sess & history . I.value %~ RB.update hist_index (init left ++ right)
                       & moveCursor (-1)
    where hist_index = sess ^. history . I.index
          input = getInput sess
          (left, right) = splitAt (sess ^. cursor) input

deleteInput :: Sess -> Sess
deleteInput sess
    | right == "" = sess
    | otherwise = sess & history . I.value %~ RB.update hist_index (left ++ tail right)
    where hist_index = sess ^. history . I.index
          input = getInput sess
          (left, right) = splitAt (sess ^. cursor) input

clearInput :: Sess -> Sess
clearInput sess = sess & history . I.value %~ RB.update hist_index ""
                       & cursor .~ 0
    where hist_index = sess ^. history . I.index

moveCursor :: Int -> Sess -> Sess
moveCursor n sess = sess & cursor %~ clamp 0 (length $ getInput sess) . (+) n

bind :: V.Event -> (Sess -> EventM () (Next Sess)) -> Sess -> Sess
bind event action = bindings %~ Map.insert event action

writeBuffer :: Int -> FString -> Sess -> Sess
writeBuffer bnum = flip (foldl' $ addBufferChar bnum)

writeBufferLn :: Int -> FString -> Sess -> Sess
writeBufferLn bnum str = writeBuffer bnum (str ++ [FChar { _ch = '\n', _attr = V.defAttr}])

logError :: String -> Sess -> Sess
logError msg = writeBufferLn debugBufferNum . colorize V.red $ msg

logInfo :: String -> Sess -> Sess
logInfo msg = writeBufferLn debugBufferNum . colorize V.magenta $ msg

switchToBuffer :: Int -> Sess -> Sess
switchToBuffer bnum = buffers . I.index .~ bnum

toggleBuffer :: Sess -> Sess
toggleBuffer sess = switchToBuffer new_buf_num sess
    where curr_buf_num = currentBufferNum sess
          new_buf_num = if curr_buf_num == debugBufferNum then
                           mainBufferNum
                        else debugBufferNum

-- buffer number, highlight line, start index, end index.
highlightStr :: Int -> (Int, Int, Int) -> Sess -> Sess
highlightStr bnum = flip (modifyBuffer bnum) $ withStyle V.standout

-- buffer number, unhighlight line, start index, end index.
unhighlightStr :: Int -> (Int, Int, Int) -> Sess -> Sess
unhighlightStr bnum = flip (modifyBuffer bnum) $ withStyle V.defaultStyleMask

-- Searches the specified buffer.
searchBackwards :: Int -> String -> Sess -> Sess
searchBackwards bnum str sess
    | Left err <- regex_res = sess & logError ("Regex error: " ++ err)
    | Right regex <- regex_res =
         case searchBackwardsHelper regex (curr_buf ^. buffer . I.value) start_line of
             Just sr@(line,_,_) -> sess & unhighlightPrevious
                                        & highlightStr bnum sr
                                        & setSearchRes str (Just sr)
                                        & scrollLines (line - curr_buf_scroll)
             Nothing -> sess -- & logError ("Search string not found: " ++ str)
                             & unhighlightPrevious
                             & setSearchRes str Nothing
                             & (nBuffer bnum) . buffer . I.index .~ 0
    where regex_res = R.compile regexCompOpt regexExecOpt str
          curr_buf = fromJust $ sess ^? (nBuffer bnum)
          curr_buf_scroll = (curr_buf ^. buffer . I.index)
          lsearch = curr_buf ^. last_search
          start_line
              | Just sr <- lsearch =
                    if (sr ^. search) == str then (sr ^. line) + 1 else 0
              | otherwise = 0
          unhighlightPrevious sess
              | Just sr <- lsearch =
                    unhighlightStr bnum
                        ((sr ^. line), (sr ^. start), (sr ^. end)) $ sess
              | otherwise = sess
          setSearchRes str (Just (line, start, end)) sess =
              sess & (nBuffer bnum) . last_search .~ Just (searchResult str line start end)
          setSearchRes _ Nothing sess = sess & (nBuffer bnum) . last_search .~ Nothing

scrollLines :: Int -> Sess -> Sess
scrollLines n = currentBuffer . buffer . I.index %~ (+) n

pageUp :: Sess -> Sess
pageUp = scrollLines 10

pageDown :: Sess -> Sess
pageDown = scrollLines $ -10

addToHistory :: String -> Sess -> Sess
addToHistory s sess =
    sess & history . I.value %~ RB.push "" . RB.update 0 s
         & historyNewest
         & cursor .~ 0

scrollHistory :: Int -> Sess -> Sess
scrollHistory n sess = sess &
    history . I.index %~ (+) n &
    moveCursor 999999 -- big arbitrary number to get to the end of the line

historyOlder :: Sess -> Sess
historyOlder = scrollHistory 1

historyNewer :: Sess -> Sess
historyNewer = scrollHistory $ -1

historyNewest :: Sess -> Sess
historyNewest = history . I.index .~ 0

sendToServer :: String -> Sess -> IO Sess
sendToServer str sess = do
    atomically $ writeTQueue (sess ^. send_queue) (BSC.pack $ str ++ "\r\n")
    return sess

sendRawToServer :: [Word8] -> Sess -> IO Sess
sendRawToServer bytes sess = do
    atomically $ writeTQueue (sess ^. send_queue) $ BS.pack bytes
    return sess

sendGmcp :: String -> Sess -> IO Sess
sendGmcp s = sendRawToServer $ [tIAC, tSB, tGMCP] ++
                               (BS.unpack . BSC.pack $ s) ++
                               [tIAC, tSE]

receiveServerData :: Sess -> BS.ByteString -> Sess
receiveServerData sess bs =
    sess & recv_state %~ \rs -> foldl handleServerByte rs $ BS.unpack bs

-- === HELPER FUNCTIONS ===
-- Lenses for buffer access.
mainBufferNum = 1
debugBufferNum = 0
currentBufferNum sess = (sess ^. buffers . I.index)
nBuffer :: (Applicative f, Functor f) => Int -> (Buffer -> f Buffer) -> Sess -> f Sess
nBuffer idx = buffers . I.value . ix idx
currentBuffer func sess = nBuffer (currentBufferNum sess) func sess

-- buffer number, (line, start index, end index), modification func, session
modifyBuffer :: Int -> (Int, Int, Int) -> (FChar -> FChar) -> Sess -> Sess
modifyBuffer bnum (line, start, end) func sess =
    sess & nBuffer bnum . buffer . I.value %~ RB.update line new_str
    where line_str = (fromJust $ sess ^? nBuffer bnum . buffer . I.value) RB.! line
          replaceAtIndex f n ls = a ++ ((f item):b)
              where (a, (item:b)) = splitAt n ls
          new_str = (foldr (flip (.)) id $ flip map [start..end] $
                    replaceAtIndex func) line_str

-- Input: Search string, buffer, starting line.
-- Returns: Line, start index, end index if found; Nothing otherwise.
searchBackwardsHelper :: R.Regex -> RB.RingBuffer FString -> Int -> Maybe (Int, Int, Int)
searchBackwardsHelper r buf start_line
    | Just idx <- first_found =
          let (start, len) = fromJust (S.index search_results idx)
          in Just (start_line + idx, start, start + len - 1)
    | otherwise = Nothing
    where search_results = fmap (findInFString r) . RB.drop start_line $ buf
          first_found = S.findIndexL isJust search_results

-- Returns (start, length) if found; Nothing otherwise.
findInFString :: R.Regex -> FString -> Maybe (Int, Int)
findInFString r fs =
    case R.execute r $ removeFormatting fs of
         Left _ -> Nothing
         Right Nothing -> Nothing
         Right (Just ma) -> Just $ ma ! 0

handleServerByte :: RecvState -> Word8 -> RecvState
handleServerByte recv_state b
    | t@(InProgress _) <- new_telnet = recv_state & telnet_state .~ t
    | t@(Success cmd) <- new_telnet = recv_state & telnet_state .~ t
                                                 & telnet_cmds %~ flip (++) [cmd]
    | e@(InProgress _) <- new_esc_seq = recv_state & esc_seq_state .~ e
    | e@(Success seq) <- new_esc_seq = recv_state & esc_seq_state .~ e
                                                  & char_attr %~ \ca ->
                                                                   updateCharAttr ca seq
    | otherwise = recv_state & text %~ ((:)
        FChar { _ch = BSC.head . BS.singleton $ b , _attr = (recv_state ^. char_attr)})
    where new_telnet = parseTelnet (recv_state ^. telnet_state) b
          new_esc_seq = parseEscSeq (recv_state ^. esc_seq_state) b

-- buffer number, session, char
addBufferChar :: Int -> Sess -> FChar -> Sess
addBufferChar bnum sess c
    -- Newlines move to next line.
    | (_ch c) == '\n' = sess & nBuffer bnum . buffer . I.value %~ RB.push emptyF
                             & nBuffer bnum . last_search %~ updateSearchResult
                             & updateScrollLoc
    -- Throw out carriage returns.
    | (_ch c) == '\r' = sess
    -- Add char to end of last line.
    | otherwise = sess & nBuffer bnum . buffer . I.value %~ RB.update 0
          (((fromJust $ sess ^? nBuffer bnum . buffer . I.value) RB.! 0) ++ [c])
    where updateSearchResult :: Maybe SearchResult -> Maybe SearchResult
          updateSearchResult Nothing = Nothing
          updateSearchResult (Just res)
              -- Search result will get pushed out of the buffer; flush it.
              | (res ^. line) + 1 >= RB.length
                    (fromJust $ sess ^? nBuffer bnum . buffer . I.value) = Nothing
              -- Account for new line in the search result.
              | otherwise = Just $ res & line %~ (+1)
          updateScrollLoc :: Sess -> Sess
          updateScrollLoc sess
              | (fromJust $ sess ^? nBuffer bnum . buffer . I.index) == 0 = sess
              | otherwise = scrollLines 1 sess
