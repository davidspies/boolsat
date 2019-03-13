module BoolSat.Solver.CDCL.Monad
  ( AssignInfo(..)
  , AssignedLiterals(..)
  , CDCLM
  , Conflict(..)
  , MonadReadAssignment(..)
  , MonadWriteAssignment(..)
  , MonadReadRules(..)
  , MonadWriteRules(..)
  , RuleSet(..)
  , getSolutions
  , module X
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map

import           BoolSat.Solver.CDCL.LevelErrors
                                               as X
import           BoolSat.Solver.CDCL.Yield     as X
import           BoolSat.Data

data RuleSet = RuleSet
  { original :: Problem
  , learned :: [Disjunction]
  }

newtype AssignedLiterals = AssignedLiterals (Map Variable AssignInfo)
data Conflict = Conflict
  { conflictCause :: Disjunction
  , conflictLevel :: Level
  }
  deriving (Show)

data AssignInfo = AssignInfo
  { value :: Sign
  , assignLevel :: Level
  , cause :: Maybe Disjunction
  }

newtype CDCLM a = CDCLM
    {unCDCL :: StateT AssignedLiterals
                  (LevelErrorsT Conflict
                    (StateT RuleSet
                      (YieldM Solution)
                    )
                  )
                a
      }
  deriving ( Functor, Applicative, Monad
           , MonadReadLevel
           , MonadThrowLevel Conflict
           , MonadHasLevels Conflict
           , MonadYield Solution
           , MonadReadAssignment
           , MonadWriteAssignment
           , MonadReadRules
           , MonadWriteRules
           )

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

class Monad m => MonadReadRules m where
  getRules :: m RuleSet
instance (MonadTrans t, MonadReadRules m, Monad (t m))
    => MonadReadRules (Transformed t m) where
  getRules = lift getRules
instance {-# OVERLAPS #-} Monad m => MonadReadRules (StateT RuleSet m) where
  getRules = State.get
deriving via (Transformed (StateT s) m) instance MonadReadRules m
    => MonadReadRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadReadRules m
    => MonadReadRules (LevelErrorsT err m)

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()
instance (MonadTrans t, MonadWriteRules m, Monad (t m))
    => MonadWriteRules (Transformed t m) where
  addRule = lift . addRule
instance {-# OVERLAPS #-} Monad m => MonadWriteRules (StateT RuleSet m) where
  addRule r =
    State.modify (\RuleSet {..} -> RuleSet { learned = r : learned, .. })
deriving via (Transformed (StateT s) m) instance MonadWriteRules m
    => MonadWriteRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadWriteRules m
    => MonadWriteRules (LevelErrorsT err m)

instance Levelable Conflict where
  level = conflictLevel

unassigned :: AssignedLiterals
unassigned = AssignedLiterals Map.empty

getSolutions :: Problem -> CDCLM a -> [Solution]
getSolutions prob =
  execYieldM
    . (`evalStateT` RuleSet prob [])
    . runLevelErrorsT
    . (`evalStateT` unassigned)
    . unCDCL
