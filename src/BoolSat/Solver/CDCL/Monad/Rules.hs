module BoolSat.Solver.CDCL.Monad.Rules
  ( MonadReadRules(..)
  , MonadWriteRules(..)
  )
where

import           DSpies.Prelude

import qualified Data.Map                      as Map
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.ST.Class         ( liftST )
import qualified Control.Monad.State           as State
import           Data.STRef
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.Internal

class Monad m => MonadReadRules m where
  getBaseClauses :: m [Disjunction]
  getLearntClauses :: m [Disjunction]
  getAllClauses :: m [Disjunction]
  getAllClauses = (++) <$> getBaseClauses <*> getLearntClauses

class MonadReadRules m => MonadWriteRules m where
  addRule :: Disjunction -> m ()

instance MonadReadRules (CDCL s) where
  getBaseClauses =
    mapM (fmap makeDisjunction . liftST . readSTRef) =<< Reader.asks baseClauses
  getLearntClauses = mapM (fmap makeDisjunction . liftST . readSTRef)
    =<< State.gets learntClauses

makeDisjunction :: DisjunctInfo s -> Disjunction
makeDisjunction (DisjunctInfo m) = Disjunction
  $ Set.fromList (map (\(k, (v, _)) -> Assignment k v) (Map.toList m))

instance MonadWriteRules (CDCL s) where
  addRule (Disjunction (Set.toList -> as)) = do
    assigs <- Reader.asks assignments
    let newDisjunction = DisjunctInfo $ Map.fromList
          (map (\(Assignment k v) -> (k, (v, assigs Map.! k))) as)
    rref <- liftST $ newSTRef newDisjunction
    State.modify $ \CDCLState {..} ->
      CDCLState { learntClauses = rref : learntClauses, .. }
