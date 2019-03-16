module BoolSat.Solver.CDCL
  ( CDCL(..)
  , solution
  )
where

import           DSpies.Prelude

import           Control.Monad.Yield.Class

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Select
import           BoolSat.Solver.CDCL.Monad
import           BoolSat.Solver.CDCL.PureLiteralElimination
                                                ( pureLiteralElimination )
import           BoolSat.Solver.CDCL.UnitPropagation
                                                ( unitPropagation )

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
     , MonadSelect m
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
    True  -> yield =<< currentSolution
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

allSatisfied :: (MonadReadAssignment m, MonadReadRules m) => m Bool
allSatisfied = satisfies <$> currentSolution <*> (original <$> getRules)

withAssignLiteral
  :: (HasCallStack, MonadReadLevel m, MonadWriteAssignment m)
  => Assignment
  -> m a
  -> m a
withAssignLiteral (Assignment var value) act = do
  assignLevel <- askLevel
  withAssignment var (AssignInfo { cause = Nothing, .. }) act
