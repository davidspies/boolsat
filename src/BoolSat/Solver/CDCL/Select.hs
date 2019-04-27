{-# LANGUAGE UndecidableInstances #-}

module BoolSat.Solver.CDCL.Select
  ( MonadSelect(..)
  )
where

import           DSpies.Prelude

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

class MonadSelect m where
  selectLiteral :: m Assignment

instance MonadReadAssignment m => MonadSelect m where
  selectLiteral = (`Assignment` sfalse) . head <$> remainingVars
