module BoolSat.Solver.CDCL.Monad.Rules
  ( MonadReadRules(..)
  , MonadWriteRules(..)
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadRules m where
  getBaseClauses :: m [Disjunction]
  getLearntClauses :: m [Disjunction]
  getAllClauses :: m [Disjunction]
  getAllClauses = (++) <$> getBaseClauses <*> getLearntClauses

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()

instance MonadReadRules (CDCL s) where
  getBaseClauses   = CDCL $ State.gets baseClauses
  getLearntClauses = CDCL $ State.gets learntClauses

instance MonadWriteRules (CDCL s) where
  addRule r = CDCL $ State.modify $ \CDCLState {..} ->
    CDCLState { learntClauses = r : learntClauses, .. }
