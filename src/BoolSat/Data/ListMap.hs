module BoolSat.Data.ListMap where

import           BoolSat.Prelude

import qualified Data.Map                      as Map
import qualified Data.DList                    as DList

build :: Ord k => [(k, v)] -> Map k [v]
build =
  Map.map DList.toList . Map.fromListWith (<>) . map (second DList.singleton)

(!) :: Ord k => Map k [v] -> k -> [v]
(!) m k = fromMaybe [] $ m Map.!? k
