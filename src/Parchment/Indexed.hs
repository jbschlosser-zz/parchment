{-# LANGUAGE TemplateHaskell #-}

module Parchment.Indexed
    ( Indexed(..)
    , atCurrIndex
    , indexed
    , value
    , bounds_func
    , index
    ) where

import Lens.Micro ((^.), Lens')
import Parchment.Util

data Indexed a = Indexed
    { _value :: a
    , _index :: Int
    , _bounds_func :: (a -> (Int, Int))
    }

-- Lenses.
index :: Lens' (Indexed a) Int
index f (Indexed { _value = val, _index = idx, _bounds_func = bf}) =
    fmap (\new_idx -> Indexed { _value = val, _index = new_idx, _bounds_func = bf}) (cf idx)
    where cf = fmap (clampToBounds $ bf val) . f

value :: Lens' (Indexed a) a
value f (Indexed { _value = val, _index = idx, _bounds_func = bf}) =
    fmap (\new_val -> Indexed { _value = new_val, _index = idx, _bounds_func = bf}) (f val)

bounds_func :: Lens' (Indexed a) (a -> (Int, Int))
bounds_func f (Indexed { _value = val, _index = idx, _bounds_func = bf}) =
    fmap (\new_bf -> Indexed
                     { _value = val
                     , _index = clampToBounds (new_bf val) idx -- apply new bounds
                     , _bounds_func = new_bf}) (f bf)

-- Other functions.
indexed :: a -> (a -> (Int, Int)) -> Indexed a
indexed value bounds_func = Indexed
    { _value = value
    , _index = 0
    , _bounds_func = bounds_func
    }

atCurrIndex :: (a -> Int -> b) -> Indexed a -> b
atCurrIndex idx_func i = idx_func (i ^. value) (i ^. index)

clampToBounds :: (Int, Int) -> Int -> Int
clampToBounds (imin, imax) val = clampExclusive imin imax val
