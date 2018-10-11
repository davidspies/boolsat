module Data.ListMap where

import           Data.Bifunctor                 ( second )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.DList                    as DList

build :: Ord k => [(k, v)] -> Map k [v]
build =
  Map.map DList.toList . Map.fromListWith (<>) . map (second DList.singleton)

(!) :: Ord k => Map k [v] -> k -> [v]
(!) m k = fromMaybe [] $ m Map.!? k
