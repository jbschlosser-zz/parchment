{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Parchment.WorldMap
    ( Node
    , Edge(..)
    , WorldMap(..)
    , empty
    , makeEdge
    -- lenses
    , source
    , dest
    , tag
    , nodes
    , edges
    ) where

import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as Map
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

type Node = Map.Map String String
data Edge = Edge {_source :: Int, _dest :: Int, _tag :: String}
    deriving (Eq, Generic)
data WorldMap = WorldMap {_nodes :: Map.Map Int Node, _edges :: HS.HashSet Edge}
makeLenses ''Edge
makeLenses ''WorldMap

instance Hashable Edge

empty :: WorldMap
empty = WorldMap {_nodes = Map.empty, _edges = HS.empty}

makeEdge :: (Int, Int, String) -> Edge
makeEdge (s, d, t) = Edge {_source = s, _dest = d, _tag = t}
