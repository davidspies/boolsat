{-# LANGUAGE UndecidableInstances #-}

module BoolSat.Solver.CDCL.Monad.Internal
  ( CDCLM
  , Conflict(..)
  , MonadReadRules(..)
  , MonadWriteRules(..)
  , RuleSet(..)
  , getSolutions
  )
where

import           DSpies.Prelude

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST.Class
import qualified Control.Monad.State           as State
import           Control.Monad.Yield.Class      ( MonadYield )
import           Control.Monad.Yield.ST
import           Data.STRef

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Assignment
import           BoolSat.Solver.CDCL.Monad.LevelErrors
import           BoolSat.Solver.CDCL.Monad.Mask

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
    {unCDCL :: StateT VarState
                  (LevelErrorsT Conflict
                    (ReaderT (STRef s RuleSet)
                      (YieldST s Solution)
                    )
                  )
                a
      }
  deriving ( Functor, Applicative, Monad
           , MonadMaskBase
           , MonadReadLevel
           , MonadThrowLevel Conflict
           , MonadHasLevels Conflict
           , MonadYield Solution
           , MonadReadAssignment
           , MonadWriteAssignment
           , MonadST
           )

class Monad m => MonadReadRules m where
  getRules :: m RuleSet
instance (MonadTrans t, MonadReadRules m, Monad (t m))
    => MonadReadRules (Transformed t m) where
  getRules = lift getRules
deriving via (Transformed (StateT s) m) instance MonadReadRules m
    => MonadReadRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadReadRules m
    => MonadReadRules (LevelErrorsT err m)

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()
instance (MonadTrans t, MonadWriteRules m, Monad (t m))
    => MonadWriteRules (Transformed t m) where
  addRule = lift . addRule

deriving via (Transformed (StateT s) m) instance MonadWriteRules m
    => MonadWriteRules (StateT s m)
deriving via (Transformed (LevelErrorsT err) m) instance MonadWriteRules m
    => MonadWriteRules (LevelErrorsT err m)

instance MonadReadRules (CDCLM s) where
  getRules = liftST . readSTRef =<< CDCLM Reader.ask

instance MonadWriteRules (CDCLM s) where
  addRule r = do
    ref <- CDCLM Reader.ask
    liftST $ modifySTRef
      ref
      (\RuleSet {..} -> RuleSet { learned = r : learned, .. })

instance Levelable Conflict where
  level = conflictLevel

getSolutions :: Problem -> (forall s . CDCLM s a) -> [Solution]
getSolutions prob act = runYieldST $ do
  ref <- liftST $ newSTRef $ RuleSet prob []
  (`runReaderT` ref) $ runLevelErrorsT $ (`evalStateT` unassigned prob) $ unCDCL
    act
