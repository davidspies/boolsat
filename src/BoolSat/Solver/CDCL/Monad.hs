module BoolSat.Solver.CDCL.Monad
  ( allRules
  , currentSolution
  , module X
  )
where

import           DSpies.Prelude

import qualified Data.Map                      as Map

import           BoolSat.Solver.CDCL.Monad.Internal
                                               as X
import           BoolSat.Data

allRules :: MonadReadRules m => m [Disjunction]
allRules = do
  RuleSet { original = Problem orig, learned } <- getRules
  return $ orig ++ learned

currentSolution :: MonadReadAssignment m => m Solution
currentSolution =
  getAssignment <&> \(AssignedLiterals m) -> Solution $ Map.map value m
