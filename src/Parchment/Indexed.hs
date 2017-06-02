{-# LANGUAGE TemplateHaskell #-}

module Parchment.Indexed
    ( Indexed(..)
    , adjustIndex
    , bounds_func
    , atCurrIndex
    , getIndex
    , setIndex
    , indexed
    , value
    ) where

import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)
import Parchment.Util

data Indexed a = Indexed
    { _value :: a
    , _index :: Int
    , _bounds_func :: (a -> (Int, Int))
    }
makeLenses ''Indexed

indexed :: a -> (a -> (Int, Int)) -> Indexed a
indexed value bounds_func = Indexed
    { _value = value
    , _index = 0
    , _bounds_func = bounds_func
    }

bounds :: Indexed a -> (Int, Int)
bounds i = (i ^. bounds_func) (i ^. value)

atCurrIndex :: (a -> Int -> b) -> Indexed a -> b
atCurrIndex idx_func i = idx_func (i ^. value) (i ^. index)

getIndex :: Indexed a -> Int
getIndex i = i ^. index

setIndex :: Int -> Indexed a -> Indexed a
setIndex idx i = i & index .~ new_idx
    where (imin, imax) = bounds i
          new_idx = clampExclusive imin imax idx

adjustIndex :: Int -> Indexed a -> Indexed a
adjustIndex n i = setIndex (i ^. index + n) i
