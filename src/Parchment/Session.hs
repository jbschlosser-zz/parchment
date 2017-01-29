{-# LANGUAGE TemplateHaskell #-}

module Parchment.Session
    ( Sess(..)
    , initialSession
    , addKey
    , delKey
    , sendToServer
    , clearInputLine
    , writeBuffer
    , writeBufferLn
    , getInput
    , nextHistory
    , receiveServerData
    , pageUp
    , pageDown
    , scrollLines
    , historyOlder
    , historyNewer
    , scrollHistory
    , highlightStr
    , unhighlightStr
    , searchBackwards
    ) where

import Brick.Types (EventM, Next)
import Brick.Util (clamp)
import Control.Concurrent.STM.TQueue
import Control.Monad.State (get, put, execState)
import Control.Monad.STM (atomically)
import Data.Array ((!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl', findIndex)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Language.Scheme.Types
import Lens.Micro ((.~), (^.), (&), (%~), ix)
import Lens.Micro.TH (makeLenses)
import Parchment.FString
import Parchment.Parsing
import Text.Parsec hiding (Error, getInput)
import Text.Regex.TDFA (CompOption(..), ExecOption(..))
import qualified Text.Regex.TDFA.String as R

data Sess = Sess {
    _buffer :: [FString]
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
    , _search_result :: Maybe (Int, Int, Int) -- line, start index, end index
    }
makeLenses ''Sess

-- Initial state of the session data.
initialSession :: TQueue BS.ByteString ->
    Map.Map V.Event (Sess -> EventM () (Next Sess)) -> Env -> Sess
initialSession q bindings scm_env =
    Sess {
        _buffer = [[]]
        , _cursor = 0
        , _bindings = bindings
        , _send_queue = q
        , _telnet_state = NotInProgress
        , _esc_seq_state = NotInProgress
        , _char_attr = V.defAttr
        , _scroll_loc = 0
        , _scroll_limit = 10000 -- lines in buffer
        , _history = [""]
        , _history_loc = 0
        , _scm_env = scm_env
        , _search_result = Nothing
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

writeBuffer :: FString -> Sess -> Sess
writeBuffer str sess = sess & buffer .~
    foldl' (addBufferChar $ sess ^. scroll_limit) (sess ^. buffer) str

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
         Right regex -> case searchBackwardsHelper regex (sess ^. buffer) (line - 1) of
                             Just sr -> highlightStr sr . setSearchRes (Just sr) .
                                 unhighlightPrevious $ sess
                             Nothing -> writeBufferLn (colorize V.red $ "Not found!") .
                                 setSearchRes Nothing . unhighlightPrevious $ sess
    where (line, _, _) = fromMaybe (length (sess ^. buffer) - 1, 0, 0) $
                             sess ^. search_result
          unhighlightPrevious sess =
              case (sess ^. search_result) of
                   Nothing -> sess
                   Just sr -> unhighlightStr sr $ sess
          setSearchRes sr sess = sess & search_result .~ sr

scrollLines :: Int -> Sess -> Sess
scrollLines n sess = sess & scroll_loc %~
    (\sl -> clampExclusive 0 (length $ sess ^. buffer) $ sl + n)

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
    atomically $ writeTQueue (sess ^. send_queue) (BSC.pack $ str ++ "\r\n")
    return sess

receiveServerData :: Sess -> BS.ByteString -> Sess
receiveServerData sess bs = foldl' handleServerByte sess $ BS.unpack bs

-- === HELPER FUNCTIONS ===
-- (line, start index, end index), modification func, session
modifyBuffer :: (Int, Int, Int) -> (FChar -> FChar) -> Sess -> Sess
modifyBuffer (line, start, end) func =
    foldr (flip (.)) id $ flip map [start..end] $
        \i -> \sess -> sess & (buffer . ix line . ix i) %~ func

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
searchBackwardsHelper :: R.Regex -> [FString] -> Int -> Maybe (Int, Int, Int)
searchBackwardsHelper r buf start_line =
    case findIndex isJust search_results of
         Nothing -> Nothing
         Just idx -> Just (start_line - idx, start, start + len - 1)
             where (start, len) = fromJust (search_results !! idx)
    where search_results = map (findInFString r) . reverse . take (start_line + 1) $ buf

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
                             put (sess & buffer .~ addBufferChar
                                 (sess ^. scroll_limit) (sess ^. buffer) (FChar
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

addBufferChar :: Int -> [FString] -> FChar -> [FString]
addBufferChar limit buf c =
    if (_ch c) == '\n' then
        -- Add new empty line.
        if length buf >= limit then
            (tail buf) ++ [[]]
        else
            buf ++ [[]]
    else if (_ch c) == '\r' then
        buf
    else if length buf == 0 then
        -- Init with char.
        [[c]]
    else
        -- Add char to end of last line.
        init buf ++ [(last buf ++ [c])]

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
