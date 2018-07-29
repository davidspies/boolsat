module BoolSat.SomeSolver
  ( SomeSolver(..)
  )
where

import           Data.Bifunctor                 ( first )

import           BoolSat.Data                   ( Solver )
import           BoolSat.Solver.Naive           ( Naive )
import           BoolSat.Solver.Partial         ( Partial )
import           BoolSat.Solver.DPLL            ( DPLL )

data SomeSolver = forall solver. (Show solver, Solver solver)
  => Some solver

instance Show SomeSolver where
  showsPrec d (Some s) = showsPrec d s

instance Read SomeSolver where
  readsPrec d r =
    (first Some <$> (readsPrec d r :: [(Naive, String)]))
      ++ (first Some <$> (readsPrec d r :: [(Partial, String)]))
      ++ (first Some <$> (readsPrec d r :: [(DPLL, String)]))
