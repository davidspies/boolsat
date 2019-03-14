module BoolSat.Solver.CDCL.Assignment
  ( AssignInfo(..)
  , AssignedLiterals(..)
  , MonadReadAssignment(..)
  , MonadWriteAssignment(..)
  , unassigned
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map

import           BoolSat.Data
import           BoolSat.Solver.CDCL.LevelErrors
                                                ( Level )

newtype AssignedLiterals = AssignedLiterals (Map Variable AssignInfo)

data AssignInfo = AssignInfo
  { value :: Sign
  , assignLevel :: Level
  , cause :: Maybe Disjunction
  }

class Monad m => MonadReadAssignment m where
  getAssignment :: m AssignedLiterals
instance Monad m => MonadReadAssignment (StateT AssignedLiterals m) where
  getAssignment = State.get

class MonadReadAssignment m => MonadWriteAssignment m where
  addAssignment :: HasCallStack => Variable -> AssignInfo -> m ()
  withAssignment :: Variable -> AssignInfo -> m a -> m a
instance Monad m => MonadWriteAssignment (StateT AssignedLiterals m) where
  addAssignment k v = State.modify
    (\(AssignedLiterals m) ->
      AssignedLiterals $ Map.insertWith (error "already present") k v m
    )
  withAssignment k v act = do
    current <- State.get
    addAssignment k v
    result <- act
    State.put current
    return result

unassigned :: AssignedLiterals
unassigned = AssignedLiterals Map.empty
