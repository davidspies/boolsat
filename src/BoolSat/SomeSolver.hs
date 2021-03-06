module BoolSat.SomeSolver
  ( SomeSolver(..)
  )
where

import           DSpies.Prelude

import           BoolSat.Data                   ( Solver )
import           BoolSat.Solver.CDCL            ( CDCL )
import           BoolSat.Solver.DPLL            ( DPLL )
import           BoolSat.Solver.Naive           ( Naive )
import           BoolSat.Solver.Partial         ( Partial )

data SomeSolver = forall solver. (Show solver, Solver solver)
  => Some solver

instance Show SomeSolver where
  showsPrec d (Some s) = showsPrec d s

instance Read SomeSolver where
  readsPrec d r =
    (first Some <$> (readsPrec d r :: [(Naive, String)]))
      ++ (first Some <$> (readsPrec d r :: [(Partial, String)]))
      ++ (first Some <$> (readsPrec d r :: [(DPLL, String)]))
      ++ (first Some <$> (readsPrec d r :: [(CDCL, String)]))
