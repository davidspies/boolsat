module BoolSat.Solver.CDCL.Monad.Conflict
  ( MonadCatchConflict(..)
  , MonadReadLevel(..)
  , MonadThrowConflict(..)
  )
where

import           DSpies.Prelude

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST.Class
import qualified Control.Monad.State           as State
import           Data.List.NonEmpty             ( (<|) )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map                      as Map
import           Data.STRef

import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadLevel m where
  askLevel :: m Level

class MonadReadLevel m => MonadThrowConflict m where
  throwConflict :: Conflict -> m a

class MonadThrowConflict m => MonadCatchConflict m where
  onNextLevel :: m a -> m (Either Conflict a)

instance MonadReadLevel (CDCL s) where
  askLevel = State.gets level

instance MonadThrowConflict (CDCL s) where
  throwConflict = throwError

instance MonadCatchConflict (CDCL s) where
  onNextLevel act = do
    State.modify $ \CDCLState {..} -> CDCLState
      { level       = incrLevel level
      , assignTimes = [] <| assignTimes
      , ..
      }
    result <- CDCL $ lift $ runExceptT $ unCDCL act
    h :| t <- State.gets assignTimes
    assigs <- Reader.asks assignments
    State.modify $ \CDCLState {..} -> CDCLState
      { level       = decrLevel level
      , assignTimes = NonEmpty.fromList t
      , ..
      }
    forM_ h $ \k -> liftST $ modifySTRef (assigs Map.! k) $ \VarInfo {..} ->
      maybe (error "Already unassigned")
            (const VarInfo { assigned = Nothing, .. })
            assigned
    case result of
      Left c -> do
        l <- State.gets level
        if conflictLevel c > l then return result else throwError c
      Right _ -> return result
