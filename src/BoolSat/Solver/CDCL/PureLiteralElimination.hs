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

pureLiteralElimination
  :: (HasCallStack, MonadWriteAssignment m, MonadReadRules m) => m ()
pureLiteralElimination = do
  Problem origs <- original <$> getRules
  remaining     <- filterM (fmap not . constraintSatisfied) origs
  let litState = Map.unionsWith (<>) $ map presences remaining
  sequence_ $ Map.mapWithKey assignBy litState
 where
  pureAssign var value = do
    AssignedLiterals m <- getAssignment
    unless (var `Map.member` m) $ addAssignment
      var
      AssignInfo { value
                 , assignLevel = error "unused level"
                 , cause       = error "unused cause"
                 }
  assignBy var = \case
    AsFalse -> pureAssign var sfalse
    AsTrue  -> pureAssign var strue
    AsBoth  -> return ()

constraintSatisfied :: MonadReadAssignment m => Disjunction -> m Bool
constraintSatisfied d = (`satisfiesConstraint` d) <$> currentSolution
