module Parchment.Util
    ( IOMaybe
    , runIOMaybe
    , returnMaybe
    , chainM
    ) where

import Control.Monad (liftM, ap)
import Control.Monad.IO.Class

newtype IOMaybe a = IOM { runIOMaybe :: IO (Maybe a) }

instance Monad IOMaybe where
    -- bind operator
    (IOM ioa) >>= f = IOM $ do
        a <- ioa
        case a of
            Nothing -> return Nothing
            Just v  -> runIOMaybe (f v)
    -- return
    return a = IOM (return (Just a))

instance MonadIO IOMaybe where
    liftIO ioa = IOM $ do
        v <- ioa
        return (Just v)

instance Functor IOMaybe where
    fmap = liftM

instance Applicative IOMaybe where
    pure  = return
    (<*>) = ap

returnMaybe :: Maybe a -> IOMaybe a
returnMaybe ma = IOM (return ma)

-- Chain a list of monad functions, with each result feeding into the next.
chainM :: (Monad m) => [a -> m a] -> a -> m a
chainM [] a = return a
chainM (x:xs) a = do
    res <- x a
    chainM xs res
