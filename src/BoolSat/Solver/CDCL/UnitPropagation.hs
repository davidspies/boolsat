module BoolSat.Solver.CDCL.UnitPropagation
  ( unitPropagation
  )
where

import           DSpies.Prelude          hiding ( throwError )

import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

unitPropagation
  :: (MonadReadRules m, MonadWriteAssignment m, MonadThrowLevel Conflict m)
  => m ()
unitPropagation = do
  rs      <- allRules
  changed <- or <$> mapM tryUnitClause rs
  when changed unitPropagation

data Remaining
    = NoneRemaining | OneRemaining Assignment | ManyRemaining | Satisfied
  deriving (Eq, Ord)

instance Semigroup Remaining where
  (<>) (OneRemaining _) (OneRemaining _) = ManyRemaining
  (<>) x                y                = max x y

instance Monoid Remaining where
  mempty = NoneRemaining

countRemainingTerms :: MonadReadAssignment m => Disjunction -> m Remaining
countRemainingTerms (Disjunction lits) = do
  VarState m <- getAssignment
  return $ foldMap
    (\a@(Assignment var val) -> case m Map.! var of
      Unassigned _ -> OneRemaining a
      Assigned AssignInfo { value } | val == value -> Satisfied
      Assigned _   -> NoneRemaining
    )
    lits

tryUnitClause
  :: (MonadWriteAssignment m, MonadThrowLevel Conflict m)
  => Disjunction
  -> m Bool
tryUnitClause d = countRemainingTerms d >>= \case
  NoneRemaining -> do
    lvl           <- maxLevel d
    newConstraint <- makeClause d lvl
    throwError $ Conflict newConstraint lvl
  OneRemaining (Assignment var value) -> do
    assignLevel <- askLevel
    let cause = Just d
    addAssignment var AssignInfo { .. }
    return True
  ManyRemaining -> return False
  Satisfied     -> return False

makeClause :: MonadReadAssignment m => Disjunction -> Level -> m Disjunction
makeClause (Disjunction lits) lev = do
  VarState curState <- getAssignment
  let
    go :: Assignment -> State (Set Assignment) (Set Assignment)
    go a@(Assignment var nval) = do
      visited <- State.get
      if a `Set.member` visited
        then return Set.empty
        else do
          State.modify (Set.insert a)
          case curState Map.! var of
            Unassigned _ -> return $ Set.singleton a
            Assigned AssignInfo { value } | value == nval ->
              error "go called on wrong sign"
            Assigned AssignInfo { assignLevel } | assignLevel < lev ->
              return $ Set.singleton a
            Assigned AssignInfo { cause = Nothing } -> return $ Set.singleton a
            Assigned AssignInfo { cause = Just (Disjunction lits'), value } ->
              Set.unions <$> mapM
                (\x@(Assignment var' val') -> if var' == var
                  then if val' == value
                    then return Set.empty
                    else error "Disjunction implies opposite thing"
                  else go x
                )
                (Set.toList lits')
  return $ Disjunction $ Set.unions $ evalState (mapM go (Set.toList lits))
                                                Set.empty
maxLevel :: (HasCallStack, MonadReadAssignment m) => Disjunction -> m Level
maxLevel (Disjunction lits) = getAssignment <&> \(VarState m) -> maximum $ map
  (\(Assignment var v) ->
    let Assigned AssignInfo {..} = m Map.! var
    in  if v == value then error "Conflict signs match" else assignLevel
  )
  (Set.toList lits)
