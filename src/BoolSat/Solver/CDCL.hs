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
                                         hiding ( CDCL )
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
   . ( MonadCatchConflict m
     , MonadWriteAssignment m
     , MonadWriteRules m
     , MonadSelect m
     , MonadYield Solution m
     )
  => m ()
search = do
  unitPropagation
  pureLiteralElimination
  fix $ \act0 -> onNextLevel go >>= \case
    Left conflict -> do
      learnFrom conflict
      act0
    Right () -> return ()
 where
  go = allSatisfied >>= \case
    True  -> yield =<< currentSolution
    False -> do
      Assignment var sig <- selectLiteral
      let tryAssign :: Sign -> m (Either Conflict ())
          tryAssign s = onNextLevel $ do
            nextAssignment (Assignment var s)
            go
      tryAssign sig >>= \case
        Left conflict -> do
          learnFrom conflict
          go
        Right () -> tryAssign (opp sig) >>= \case
          Left  conflict -> learnFrom conflict
          Right ()       -> return ()

nextAssignment
  :: (MonadWriteAssignment m, MonadReadRules m, MonadThrowConflict m)
  => Assignment
  -> m ()
nextAssignment assig = do
  assignDecisionLiteral assig
  unitPropagation
  pureLiteralElimination

learnFrom
  :: (MonadWriteAssignment m, MonadThrowConflict m, MonadWriteRules m)
  => Conflict
  -> m ()
learnFrom (Conflict c _) = do
  addRule c
  unitPropagation
  pureLiteralElimination

allSatisfied :: (MonadReadAssignment m, MonadReadRules m) => m Bool
allSatisfied = satisfies <$> currentSolution <*> (Problem <$> getBaseClauses)

assignDecisionLiteral
  :: (MonadReadLevel m, MonadWriteAssignment m) => Assignment -> m ()
assignDecisionLiteral (Assignment var value) = do
  assignLevel <- askLevel
  addAssignment var (AssignInfo { cause = Nothing, .. })
