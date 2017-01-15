{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.STM.TQueue
import Control.Monad (void)
import Data.Map (fromList)
import Lens.Micro ((.~), (^.), (&), (%~), over, ix, Getting)
import Lens.Micro.TH (makeLenses)
import Parchment.Parsing
import Parchment.Session
import Test.HUnit

makeLenses ''Sess

fakeHistory = ["this", "is", "a", "test"]

fakeSession :: IO Sess
fakeSession = do
    queue <- newTQueueIO
    let sess = initialSession queue $ fromList []
    return sess

unchangedState :: (Eq a, Show a) => s -> s -> String -> Getting a s a -> Assertion
unchangedState initial final desc getter = changedState initial final desc getter id

changedState :: (Eq a, Show a) => s -> s -> String -> Getting a s a -> (a -> a) -> Assertion
changedState initial final desc getter transform  =
    assertEqual desc (transform $ initial ^. getter) (final ^. getter)

test1 = TestLabel "History properly moves forward" $ TestCase $ do
    sess <- fakeSession
    let initial = sess
         & history .~ fakeHistory
         & history_loc .~ 3
         & cursor .~ 0
    let final = nextHistory initial
    let changed = changedState initial final
    let unchanged = unchangedState initial final
    unchanged "Cursor" cursor
    changed "History" history (flip (++) [""])
    changed "History loc" history_loc (+1)

tests = TestList
    [ test1
    ]

main :: IO ()
main = void $ runTestTT tests
