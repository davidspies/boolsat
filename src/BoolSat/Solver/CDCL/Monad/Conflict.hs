module BoolSat.Solver.CDCL.Monad.Conflict
  ( MonadCatchConflict(..)
  , MonadReadLevel(..)
  , MonadThrowConflict(..)
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import           Data.List.NonEmpty             ( (<|) )
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadLevel m where
  askLevel :: m Level

class MonadReadLevel m => MonadThrowConflict m where
  throwConflict :: Conflict -> m a

class MonadThrowConflict m => MonadCatchConflict m where
  onNextLevel :: m a -> m (Either Conflict a)

instance MonadReadLevel CDCL where
  askLevel = CDCL $ State.gets level

instance MonadThrowConflict CDCL where
  throwConflict = CDCL . throwError

instance MonadCatchConflict CDCL where
  onNextLevel act = CDCL $ do
    State.modify $ \CDCLState {..} -> CDCLState
      { level       = incrLevel level
      , assignTimes = [] <| assignTimes
      , ..
      }
    result <- lift $ runExceptT $ unCDCL act
    State.modify $ \CDCLState {..} ->
      let h :| t = assignTimes
      in  CDCLState
            { assignments = assignments `Map.withoutKeys` Set.fromList h
            , level       = decrLevel level
            , assignTimes = NonEmpty.fromList t
            , ..
            }
    case result of
      Left c -> do
        l <- State.gets level
        if conflictLevel c > l then return result else throwError c
      Right _ -> return result
