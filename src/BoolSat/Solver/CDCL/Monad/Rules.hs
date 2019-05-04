module BoolSat.Solver.CDCL.Monad.Rules
  ( MonadReadRules(..)
  , MonadWriteRules(..)
  )
where

import           DSpies.Prelude

import qualified Data.Map                      as Map
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST               ( ST )
import           Control.Monad.ST.Class         ( liftST )
import qualified Control.Monad.State           as State
import           Data.STRef
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadRules m where
  getProblem :: m Problem
  getBaseClauses :: m [Disjunction]
  getLearntClauses :: m [Disjunction]
  getAllClauses :: m [Disjunction]
  getAllClauses = (++) <$> getBaseClauses <*> getLearntClauses

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()

instance MonadReadRules (CDCL s) where
  getProblem = Reader.asks originalProblem
  getBaseClauses =
    mapM (fmap makeDisjunction . liftST . readSTRef) =<< Reader.asks baseClauses
  getLearntClauses = mapM (fmap makeDisjunction . liftST . readSTRef)
    =<< State.gets learntClauses

makeDisjunction :: DisjunctInfo s -> Disjunction
makeDisjunction DisjunctInfo { info } = Disjunction
  $ Set.fromList (map (\(k, (v, _)) -> Assignment k v) (Map.toList info))

data UseCount = UseCount{satisfying :: Int, remaining :: Int}

instance Semigroup UseCount where
  (<>) (UseCount ls lr) (UseCount rs rr) = UseCount (ls + rs) (lr + rr)

instance Monoid UseCount where
  mempty = UseCount 0 0

getUseCount :: DisjunctionMap s -> ST s UseCount
getUseCount = fmap fold . traverse checkVar . Map.elems

checkVar :: (Sign, STRef s (VarInfo s)) -> ST s UseCount
checkVar (v, ref) = readSTRef ref <&> \VarInfo { assigned } -> case assigned of
  Nothing -> UseCount { satisfying = 0, remaining = 1 }
  Just AssignInfo { value } | v == value ->
    UseCount { satisfying = 1, remaining = 0 }
  Just AssignInfo{} -> UseCount { satisfying = 0, remaining = 0 }

instance MonadWriteRules (CDCL s) where
  addRule (Disjunction (Set.toList -> as)) = do
    assigs         <- Reader.asks assignments
    newDisjunctRef <- liftST $ do
      let
        info =
          Map.fromList $ map (\(Assignment k v) -> (k, (v, assigs Map.! k))) as
      UseCount { satisfying, remaining } <- getUseCount info
      newDisjunctRef <- newSTRef DisjunctInfo { info, satisfying, remaining }
      forM_ info $ \(_, v) -> modifySTRef v
        $ \vi@VarInfo { uses } -> vi { uses = newDisjunctRef : uses }
      return newDisjunctRef
    State.modify $ \state@CDCLState { learntClauses } ->
      state { learntClauses = newDisjunctRef : learntClauses }
