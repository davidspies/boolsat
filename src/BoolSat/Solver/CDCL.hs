module BoolSat.Solver.CDCL
  ( CDCL(..)
  , solution
  )
where

import           DSpies.Prelude          hiding ( throwError )

import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

data CDCL = CDCL
  deriving (Read, Show)

instance Solver CDCL where
  solve CDCL = solution

solution :: Problem -> [Solution]
solution prob = getSolutions prob search

search
  :: forall m
   . ( MonadWriteRules m
     , MonadWriteAssignment m
     , MonadHasLevels Conflict m
     , MonadYield Solution m
     )
  => m ()
search = do
  unitPropagation
  pureLiteralElimination
  let act0 = onNextLevel go >>= \case
        Left conflict -> do
          learnFrom conflict
          act0
        Right () -> return ()
  act0
 where
  go = allSatisfied >>= \case
    True  -> yield =<< getSolution
    False -> do
      Assignment var sig <- selectLiteral
      let tryAssign :: HasCallStack => Sign -> m (Either Conflict ())
          tryAssign s = onNextLevel $ nextAssignment (Assignment var s) go
      tryAssign sig >>= \case
        Left conflict -> do
          learnFrom conflict
          go
        Right () -> tryAssign (opp sig) >>= \case
          Left  conflict -> learnFrom conflict
          Right ()       -> return ()

nextAssignment
  :: ( HasCallStack
     , MonadWriteAssignment m
     , MonadReadRules m
     , MonadThrowLevel Conflict m
     )
  => Assignment
  -> m a
  -> m a
nextAssignment assig act = withAssignLiteral assig $ do
  unitPropagation
  pureLiteralElimination
  act

learnFrom
  :: (MonadWriteAssignment m, MonadThrowLevel Conflict m, MonadWriteRules m)
  => Conflict
  -> m ()
learnFrom (Conflict c _) = do
  addRule c
  unitPropagation
  pureLiteralElimination

makeClause :: MonadReadAssignment m => Disjunction -> Level -> m Disjunction
makeClause (Disjunction lits) lev = do
  AssignedLiterals curState <- getAssignment
  let
    go :: Assignment -> State (Set Assignment) (Set Assignment)
    go a@(Assignment var nval) = do
      visited <- State.get
      if a `Set.member` visited
        then return Set.empty
        else do
          State.modify (Set.insert a)
          case Map.lookup var curState of
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
  return $ Disjunction $ Set.unions $ evalState (mapM go (Set.toList lits))
                                                Set.empty

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
  AssignedLiterals m <- getAssignment
  return $ foldMap
    (\a@(Assignment var val) -> case Map.lookup var m of
      Nothing -> OneRemaining a
      Just AssignInfo { value } | val == value -> Satisfied
      Just _  -> NoneRemaining
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

maxLevel :: MonadReadAssignment m => Disjunction -> m Level
maxLevel (Disjunction lits) = getAssignment <&> \(AssignedLiterals m) ->
  maximum $ map
    (\(Assignment var v) ->
      let AssignInfo {..} = m Map.! var
      in  if v == value then error "Conflict signs match" else assignLevel
    )
    (Set.toList lits)

unitPropagation
  :: (MonadReadRules m, MonadWriteAssignment m, MonadThrowLevel Conflict m)
  => m ()
unitPropagation = do
  rs      <- allRules
  changed <- or <$> mapM tryUnitClause rs
  when changed unitPropagation

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
constraintSatisfied d = (`satisfiesConstraint` d) <$> getSolution

allRules :: MonadReadRules m => m [Disjunction]
allRules = do
  RuleSet { original = Problem orig, learned } <- getRules
  return $ orig ++ learned

getSolution :: MonadReadAssignment m => m Solution
getSolution =
  getAssignment <&> \(AssignedLiterals m) -> Solution $ Map.map value m

allSatisfied :: (MonadReadAssignment m, MonadReadRules m) => m Bool
allSatisfied = satisfies <$> getSolution <*> (original <$> getRules)

withAssignLiteral
  :: (HasCallStack, MonadReadLevel m, MonadWriteAssignment m)
  => Assignment
  -> m a
  -> m a
withAssignLiteral (Assignment var value) act = do
  assignLevel <- askLevel
  withAssignment var (AssignInfo { cause = Nothing, .. }) act

selectLiteral :: (MonadReadAssignment m, MonadReadRules m) => m Assignment
selectLiteral = do
  AssignedLiterals m                  <- getAssignment
  RuleSet { original = Problem orig } <- getRules
  return $ head $ concatMap
    (\(Disjunction rs) ->
      mapMaybe
          (\(Assignment var _) -> case Map.lookup var m of
            Nothing -> Just (Assignment var sfalse)
            Just _  -> Nothing
          )
        $ Set.toList rs
    )
    orig
