module BoolSat.Solver.CDCL.PureLiteralElimination
  ( pureLiteralElimination
  )
where

import           DSpies.Prelude

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

data Presence = AsFalse | AsTrue | AsBoth

instance Semigroup Presence where
  (<>) AsFalse AsTrue  = AsBoth
  (<>) AsTrue  AsFalse = AsBoth
  (<>) AsBoth  _       = AsBoth
  (<>) _       AsBoth  = AsBoth
  (<>) AsFalse AsFalse = AsFalse
  (<>) AsTrue  AsTrue  = AsTrue

signToPresence :: Sign -> Presence
signToPresence = \case
  Sign False -> AsFalse
  Sign True  -> AsTrue

presences :: Disjunction -> Map Variable Presence
presences (Disjunction lits) = Map.fromListWith (<>)
  $ map (\(Assignment var v) -> (var, signToPresence v)) (Set.toList lits)

pureLiteralElimination :: (MonadWriteAssignment m, MonadReadRules m) => m ()
pureLiteralElimination = do
  origs     <- getAllClauses -- TODO Should only need to look at base clauses.
  remaining <- filterM (fmap not . constraintSatisfied) origs
  let litState = Map.unionsWith (<>) $ map presences remaining
  sequence_ $ Map.mapWithKey assignBy litState

pureAssign :: MonadWriteAssignment m => Variable -> Sign -> m ()
pureAssign var value = do
  present <- isAssigned var
  unless present $ addAssignment
    var
    AssignInfo { value
               , assignLevel = error "unused level"
               , cause       = error "unused cause"
               }

assignBy :: MonadWriteAssignment m => Variable -> Presence -> m ()
assignBy var = \case
  AsFalse -> pureAssign var sfalse
  AsTrue  -> pureAssign var strue
  AsBoth  -> return ()

constraintSatisfied :: MonadReadAssignment m => Disjunction -> m Bool
constraintSatisfied d = (`satisfiesConstraint` d) <$> currentSolution
