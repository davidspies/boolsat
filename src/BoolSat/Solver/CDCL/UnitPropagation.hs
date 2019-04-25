module BoolSat.Solver.CDCL.UnitPropagation
  ( unitPropagation
  )
where

import           DSpies.Prelude          hiding ( throwError )

import qualified Control.Monad.State           as State
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

unitPropagation
  :: ( MonadReadLevel m
     , MonadReadRules m
     , MonadWriteAssignment m
     , MonadThrowConflict m
     )
  => m ()
unitPropagation = do
  rs      <- getAllClauses
  changed <- mapM tryUnitClause rs
  when (or changed) unitPropagation

data Remaining
    = NoneRemaining | OneRemaining Assignment | ManyRemaining | Satisfied
  deriving (Eq, Ord)

instance Semigroup Remaining where
  (<>) (OneRemaining _) (OneRemaining _) = ManyRemaining
  (<>) x                y                = max x y

instance Monoid Remaining where
  mempty = NoneRemaining

countRemainingTerms :: MonadReadAssignment m => Disjunction -> m Remaining
countRemainingTerms (Disjunction lits) =
  fmap fold . forM (Set.toList lits) $ \a@(Assignment var val) ->
    lookupAssignment var <&> \case
      Nothing -> OneRemaining a
      Just AssignInfo { value } | val == value -> Satisfied
      Just _  -> NoneRemaining

tryUnitClause
  :: (MonadReadLevel m, MonadWriteAssignment m, MonadThrowConflict m)
  => Disjunction
  -> m Bool
tryUnitClause d = countRemainingTerms d >>= \case
  NoneRemaining -> do
    lvl           <- maxLevel d
    newConstraint <- makeClause d lvl
    throwConflict $ Conflict newConstraint lvl
  OneRemaining (Assignment var value) -> do
    assignLevel <- askLevel
    let cause = Just d
    addAssignment var AssignInfo { .. }
    return True
  ManyRemaining -> return False
  Satisfied     -> return False

makeClause
  :: forall m . MonadReadAssignment m => Disjunction -> Level -> m Disjunction
makeClause (Disjunction lits) lev =
  Disjunction . Set.unions <$> evalStateT (mapM go (Set.toList lits)) Set.empty
 where
  go :: Assignment -> StateT (Set Assignment) m (Set Assignment)
  go a@(Assignment var nval) = do
    visited <- State.get
    if a `Set.member` visited
      then return Set.empty
      else do
        State.modify (Set.insert a)
        lookupAssignment var >>= \case
          Nothing -> return $ Set.singleton a
          Just AssignInfo { value } | value == nval ->
            error "go called on wrong sign"
          Just AssignInfo { assignLevel } | assignLevel < lev ->
            return $ Set.singleton a
          Just AssignInfo { cause = Nothing } -> return $ Set.singleton a
          Just AssignInfo { cause = Just (Disjunction lits'), value } ->
            Set.unions <$> mapM
              (\x@(Assignment var' val') -> if var' == var
                then if val' == value
                  then return Set.empty
                  else error "Disjunction implies opposite thing"
                else go x
              )
              (Set.toList lits')

maxLevel :: (HasCallStack, MonadReadAssignment m) => Disjunction -> m Level
maxLevel (Disjunction lits) = mapM levelOf (Set.toList lits) <&> \case
  []                -> level0
  litLevels@(_ : _) -> maximum litLevels
 where
  levelOf (Assignment var v) = do
    AssignInfo {..} <- getAssignment var
    return $ if v == value then error "Conflict signs match" else assignLevel
