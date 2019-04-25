module BoolSat.Solver.CDCL.Monad.Assignment
  ( MonadReadAssignment(..)
  , MonadWriteAssignment(..)
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import qualified Data.Map.Lazy                 as Map

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadAssignment m where
  getAssignment :: HasCallStack => Variable -> m AssignInfo
  getAssignment = fmap fromJust . lookupAssignment
  isAssigned :: Variable -> m Bool
  isAssigned = fmap isJust . lookupAssignment
  lookupAssignment :: Variable -> m (Maybe AssignInfo)
  currentSolution :: m Solution

instance (Monad (t m), MonadTrans t, MonadReadAssignment m)
    => MonadReadAssignment (Transformed t m) where
  getAssignment    = lift . getAssignment
  isAssigned       = lift . isAssigned
  lookupAssignment = lift . lookupAssignment
  currentSolution  = lift currentSolution

deriving via Transformed (StateT s) m instance MonadReadAssignment m
    => MonadReadAssignment (StateT s m)

instance MonadReadAssignment CDCL where
  getAssignment k = CDCL $ State.gets $ (Map.! k) . assignments
  isAssigned k = CDCL $ State.gets $ Map.member k . assignments
  lookupAssignment k = CDCL $ State.gets $ Map.lookup k . assignments
  currentSolution = CDCL $ State.gets $ Solution . Map.map value . assignments

class MonadReadAssignment m => MonadWriteAssignment m where
  addAssignment :: Variable -> AssignInfo -> m ()

instance MonadWriteAssignment CDCL where
  addAssignment k v = CDCL $ State.modify $ \CDCLState {..} -> CDCLState
    { assignments = insertNew k v assignments
    , assignTimes = mapHead (k :) assignTimes
    , ..
    }

insertNew :: (HasCallStack, Ord k) => k -> v -> Map k v -> Map k v
insertNew k v = Map.alter (maybe (Just v) (error "Already present")) k

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead fn (h :| t) = fn h :| t
