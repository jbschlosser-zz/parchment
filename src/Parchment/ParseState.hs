module Parchment.ParseState
    ( ParseState(..)
    ) where

data ParseState a = NotInProgress | InProgress a | Success a | Error a
    deriving (Show)
