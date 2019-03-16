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

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST.Class         ( MonadST )
import qualified Control.Monad.State           as State
import           Control.Monad.Yield.Class      ( MonadYield )
import           Control.Monad.Yield.ST
import           Data.STRef

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

newtype CDCLM s a = CDCLM
    {unCDCL :: StateT AssignedLiterals
                  (LevelErrorsT Conflict
                    (ReaderT (STRef s RuleSet)
                      (YieldST s Solution)
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
           , MonadST s
           )

class Monad m => MonadReadRules m where
  getRules :: m RuleSet
instance (MonadTrans t, MonadReadRules m, Monad (t m))
    => MonadReadRules (Transformed t m) where
  getRules = lift getRules
instance (Monad m, MonadST s m)
    => MonadReadRules (ReaderT (STRef s RuleSet) m) where
  getRules = liftST . readSTRef =<< Reader.ask
deriving via (Transformed (StateT s) m) instance MonadReadRules m
    => MonadReadRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadReadRules m
    => MonadReadRules (LevelErrorsT err m)

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()
instance (MonadTrans t, MonadWriteRules m, Monad (t m))
    => MonadWriteRules (Transformed t m) where
  addRule = lift . addRule
instance (Monad m, MonadST s m)
    => MonadWriteRules (ReaderT (STRef s RuleSet) m) where
  addRule r = do
    ref <- Reader.ask
    liftST $ modifySTRef
      ref
      (\RuleSet {..} -> RuleSet { learned = r : learned, .. })
deriving via (Transformed (StateT s) m) instance MonadWriteRules m
    => MonadWriteRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadWriteRules m
    => MonadWriteRules (LevelErrorsT err m)

instance Levelable Conflict where
  level = conflictLevel

getSolutions :: Problem -> (forall s . CDCLM s a) -> [Solution]
getSolutions prob act = runYieldST $ do
  ref <- liftST $ newSTRef $ RuleSet prob []
  (`runReaderT` ref) $ runLevelErrorsT $ (`evalStateT` unassigned) $ unCDCL act
