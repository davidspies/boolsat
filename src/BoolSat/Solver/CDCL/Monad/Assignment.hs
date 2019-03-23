module BoolSat.Solver.CDCL.Monad.Assignment
  ( AssignInfo(..)
  , MonadReadAssignment(..)
  , MonadWriteAssignment(..)
  , UnassignInfo(..)
  , VarInfo(..)
  , VarState(..)
  , isAssignedIn
  , unassigned
  )
where

import           DSpies.Prelude

import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.LevelErrors
                                                ( Level )

newtype VarState = VarState (Map Variable VarInfo)

data VarInfo = Assigned AssignInfo | Unassigned UnassignInfo

data UnassignInfo = UnassignInfo
  { negativeUseCount :: Int
  , positiveUseCount :: Int
  }

instance Semigroup UnassignInfo where
  (<>) (UnassignInfo nl pl) (UnassignInfo nr pr) =
    UnassignInfo (nl + nr) (pl + pr)

data AssignInfo = AssignInfo
  { value :: Sign
  , assignLevel :: Level
  , cause :: Maybe Disjunction
  }

class Monad m => MonadReadAssignment m where
  getAssignment :: m VarState
instance Monad m => MonadReadAssignment (StateT VarState m) where
  getAssignment = State.get

class MonadReadAssignment m => MonadWriteAssignment m where
  addAssignment :: HasCallStack => Variable -> AssignInfo -> m ()
  withAssignment :: Variable -> AssignInfo -> m a -> m a
instance Monad m => MonadWriteAssignment (StateT VarState m) where
  addAssignment k v = State.modify $ \(VarState m) ->
    VarState $ Map.insertWith replaceUnassigned k (Assigned v) m
   where
    replaceUnassigned x = \case
      Assigned   _ -> error "Already present"
      Unassigned _ -> x
  withAssignment k v act = do
    current <- State.get
    addAssignment k v
    result <- act
    State.put current
    return result

unassigned :: Problem -> VarState
unassigned (Problem ds) = VarState $ Map.map Unassigned $ foldl
  (Map.unionWith (<>))
  Map.empty
  (map disjunctMapping ds)
 where
  disjunctMapping (Disjunction xs) =
    Map.fromList $ map assignMapping $ Set.toList xs
  assignMapping (Assignment k v) =
    ( k
    , case v of
      Sign False -> UnassignInfo 1 0
      Sign True  -> UnassignInfo 0 1
    )

isAssignedIn :: Variable -> VarState -> Bool
isAssignedIn k (VarState m) = case m Map.! k of
  Assigned   _ -> True
  Unassigned _ -> False
