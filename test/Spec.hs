{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.STM.TQueue
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Map (fromList)
import Data.Word (Word8)
import Language.Scheme.Core
import Language.Scheme.Types hiding (bindings)
import Lens.Micro ((.~), (^.), (&), (%~), over, ix, Getting)
import Lens.Micro.TH (makeLenses)
import Parchment.FString
import Parchment.Session hiding (cursor)
import Test.HUnit

--makeLenses ''Sess

fakeSession :: IO Sess
fakeSession = do
    queue <- newTQueueIO
    scmEnv <- r5rsEnv
    let settings = defaultSettings "localhost" 4000
    let sess = initialSession settings queue (fromList []) scmEnv
    return sess

unchangedState :: (Eq a, Show a) => s -> s -> String -> Getting a s a -> Assertion
unchangedState initial final desc getter = changedState initial final desc getter id

changedState :: (Eq a, Show a) => s -> s -> String -> Getting a s a -> (a -> a) -> Assertion
changedState initial final desc getter transform  =
    assertEqual desc (transform $ initial ^. getter) (final ^. getter)

testBS :: BS.ByteString
testBS = BS.pack . concat . take 5000 . repeat $ ([65, 66, 67, 68, 69, 70] :: [Word8])

-- test1 = TestLabel "History properly moves forward" . TestCase $ do
--     sess <- fakeSession
--     let initial = sess
--          & history .~ fakeHistory
--          & history_loc .~ 3
--          & cursor .~ 0
--     let final = nextHistory initial
--     let changed = changedState initial final
--     let unchanged = unchangedState initial final
--     unchanged "Cursor" cursor
--     changed "History" history (flip (++) [""])
--     changed "History loc" history_loc (+1)

test1 = TestLabel "Benchmarking receive" . TestCase $ do
    sess <- fakeSession
    let initial = sess
    let final = receiveServerData initial testBS
    assertEqual "Reflexive" (final ^. (recv_state . text)) (final ^. (recv_state . text))

tests = TestList
    [ test1
    ]

main :: IO ()
main = void $ runTestTT tests
