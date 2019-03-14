module BoolSat.Solver.CDCL.Monad.Internal
  ( CDCLM
  , Conflict(..)
  , MonadReadRules(..)
  , MonadWriteRules(..)
  , RuleSet(..)
  , getSolutions
  , module X
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import           Control.Monad.Yield

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Assignment
                                               as X
import           BoolSat.Solver.CDCL.Monad.LevelErrors
                                               as X

data RuleSet = RuleSet
  { original :: Problem
  , learned :: [Disjunction]
  }

data Conflict = Conflict
  { conflictCause :: Disjunction
  , conflictLevel :: Level
  }
  deriving (Show)

newtype CDCLM a = CDCLM
    {unCDCL :: StateT AssignedLiterals
                  (LevelErrorsT Conflict
                    (StateT RuleSet
                      (Yield Solution)
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

getSolutions :: Problem -> CDCLM a -> [Solution]
getSolutions prob =
  runYield
    . (`evalStateT` RuleSet prob [])
    . runLevelErrorsT
    . (`evalStateT` unassigned)
    . unCDCL
