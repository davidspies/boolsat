module BoolSat.Solver.Naive
  ( Naive(Naive)
  )
where

import qualified Data.Set                      as Set

import           BoolSat.Data

data Naive = Naive
  deriving (Read, Show)

instance Solver Naive where
  solve Naive prob =
    filter (`satisfies` prob) $
    makeSolution <$> mapM possibilities (Set.toList $ allVars prob)
