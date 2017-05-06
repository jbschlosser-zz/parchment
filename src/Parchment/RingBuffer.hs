module Parchment.RingBuffer
    ( RingBuffer
    , new
    , newInit
    , push
    , length
    , capacity
    , (!)
    , updateMostRecent
    , drop
    , take
    , toSeq
    ) where

import Prelude hiding (length, drop, take)
import qualified Data.Sequence as S

newtype RingBuffer a = RB (S.Seq a, Int) deriving (Eq, Ord, Show)

-- | Create a new RingBuffer, initialized to all 0's, of the given size
new :: (Num a) => Int -> RingBuffer a
new = newInit 0
{-# INLINE new #-}

-- | Create a new RingBuffer from a given initial value
newInit :: a -> Int -> RingBuffer a
newInit _ sz | sz <= 0 = error "can't make empty ringbuffer"
newInit i sz = RB (S.replicate sz i, 0)
{-# INLINE newInit #-}

-- | Get the current size of a RingBuffer.
length :: RingBuffer a -> Int
length (RB (_, len)) = len
{-# INLINE length #-}

-- | Get the number of elements that can be store in this RingBuffer.
capacity :: RingBuffer a -> Int
capacity (RB (vec, _)) = S.length vec

-- | Look up a value in a RingBuffer.
(!) :: RingBuffer a -> Int -> a
(!) (RB (vec, len)) ind | ind >= len = error "index out of bounds"
                        | otherwise = S.index vec ind
{-# INLINE (!) #-}

drop :: Int -> RingBuffer a -> S.Seq a
drop n rb = S.drop n $ toSeq rb
{-# INLINE drop #-}

take :: Int -> RingBuffer a -> S.Seq a
take n (RB (vec, len)) = S.take (min len n) vec
{-# INLINE take #-}

toSeq :: RingBuffer a -> S.Seq a
toSeq (RB (vec, len)) = S.take len vec
{-# INLINE toSeq #-}

updateMostRecent :: RingBuffer a -> a -> RingBuffer a
updateMostRecent (RB (_, 0)) _ = error "Nothing to update in the RingBuffer"
updateMostRecent (RB (vec, len)) el = RB (S.update 0 el vec, len)
{-# INLINE updateMostRecent #-}

-- | Push a new value into a RingBuffer.  The following will hold:
--     NewRingBuffer ! 0 === added element
--     NewRingBuffer ! 1 === OldRingBuffer ! 0
push :: RingBuffer a -> a -> RingBuffer a
push (RB (vec, len)) el = case S.viewr vec of
    v' S.:> _ -> RB (el S.<| v', min (S.length vec) $ len + 1)
    _ -> error "internal error"
{-# INLINE push #-}
