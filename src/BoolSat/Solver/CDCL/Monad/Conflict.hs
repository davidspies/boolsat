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

import           BoolSat.Data
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
    setup
    result <- CDCL $ lift $ runExceptT $ unCDCL act
    cleanup
    case result of
      Left c -> do
        l <- State.gets level
        if conflictLevel c > l then return result else throwError c
      Right _ -> return result

setup :: CDCL s ()
setup = State.modify $ \state@CDCLState { level, assignTimes } ->
  state { level = incrLevel level, assignTimes = [] <| assignTimes }

cleanup :: CDCL s ()
cleanup = do
  h :| t <- State.gets assignTimes
  assigs <- Reader.asks assignments
  State.modify $ \state@CDCLState { level } ->
    state { level = decrLevel level, assignTimes = NonEmpty.fromList t }
  liftST $ forM_ h $ \k -> do
    let vr = assigs Map.! k
    vi@VarInfo { assigned, uses } <- readSTRef vr
    case assigned of
      Nothing                   -> error "Already unassigned"
      Just AssignInfo { value } -> do
        writeSTRef vr $ vi { assigned = Nothing }
        forM_ uses $ \use -> modifySTRef use (unassignAdjustCounts k value)

unassignAdjustCounts :: Variable -> Sign -> DisjunctInfo s -> DisjunctInfo s
unassignAdjustCounts k v DisjunctInfo { info, remaining, satisfying } =
  DisjunctInfo
    { info
    , remaining  = remaining + 1
    , satisfying = if v == fst (info Map.! k)
                     then satisfying - 1
                     else satisfying
    }
