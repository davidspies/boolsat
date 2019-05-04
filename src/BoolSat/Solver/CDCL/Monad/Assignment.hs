module BoolSat.Solver.CDCL.Monad.Assignment
  ( MonadReadAssignment(..)
  , MonadWriteAssignment(..)
  )
where

import           DSpies.Prelude

import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST.Class
import qualified Control.Monad.State           as State
import qualified Data.Map.Lazy                 as Map
import           Data.STRef

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadAssignment m where
  getAssignment :: HasCallStack => Variable -> m AssignInfo
  getAssignment = fmap fromJust . lookupAssignment
  isAssigned :: Variable -> m Bool
  isAssigned = fmap isJust . lookupAssignment
  lookupAssignment :: Variable -> m (Maybe AssignInfo)
  currentSolution :: m Solution
  remainingVars :: m [Variable]

instance (Monad (t m), MonadTrans t, MonadReadAssignment m)
    => MonadReadAssignment (Transformed t m) where
  getAssignment    = lift . getAssignment
  isAssigned       = lift . isAssigned
  lookupAssignment = lift . lookupAssignment
  currentSolution  = lift currentSolution
  remainingVars    = lift remainingVars

deriving via Transformed (StateT s) m instance MonadReadAssignment m
    => MonadReadAssignment (StateT s m)

instance MonadReadAssignment (CDCL s) where
  lookupAssignment k = do
    assigs <- Reader.asks assignments
    assigned <$> liftST (readSTRef $ assigs Map.! k)
  currentSolution = do
    assigs <- Reader.asks assignments
    Solution
      <$> mapFilterM (liftST . fmap (fmap value . assigned) . readSTRef) assigs
  remainingVars = do
    assigs <- Reader.asks assignments
    flip mapMaybeM (Map.toList assigs) $ \(k, vr) ->
      liftST $ readSTRef vr <&> \v ->
        if isJust (assigned v) then Nothing else Just k

mapFilterM :: Applicative m => (v -> m (Maybe w)) -> Map k v -> m (Map k w)
mapFilterM fn =
  fmap (Map.fromDistinctAscList . catMaybes)
    . traverse (\(k, v) -> fmap (k, ) <$> fn v)
    . Map.toAscList

class MonadReadAssignment m => MonadWriteAssignment m where
  addAssignment :: HasCallStack => Variable -> AssignInfo -> m ()

instance MonadWriteAssignment (CDCL s) where
  addAssignment k v = do
    assigs <- Reader.asks assignments
    liftST $ do
      let vr = assigs Map.! k
      vi@VarInfo { assigned, uses } <- readSTRef vr
      case assigned of
        Nothing -> do
          writeSTRef vr vi { assigned = Just v }
          forM_ uses $ \use -> modifySTRef use (assignAdjustCounts k (value v))
        Just _ -> error $ unwords ["variable", show k, "already set"]
    State.modify $ \state@CDCLState { assignTimes } ->
      state { assignTimes = mapHead (k :) assignTimes }

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead fn (h :| t) = fn h :| t

assignAdjustCounts :: Variable -> Sign -> DisjunctInfo s -> DisjunctInfo s
assignAdjustCounts k v DisjunctInfo { info, remaining, satisfying } =
  DisjunctInfo
    { info
    , remaining  = remaining - 1
    , satisfying = if v == fst (info Map.! k)
                     then satisfying + 1
                     else satisfying
    }
