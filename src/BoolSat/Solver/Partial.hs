module BoolSat.Solver.Partial
  ( Partial(Partial)
  )
where

import           Control.Monad
import           Control.Monad.State            ( execStateT )
import qualified Control.Monad.State           as State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data

data Partial = Partial

instance Solver Partial where
  solve Partial prob =
    filter (`satisfies` prob) $
    map makeSolution $
    (`execStateT` []) $
    forM (Set.toList $ allVars prob) $ \x -> do
      s <- State.get
      s' <- (: s) <$> State.lift (possibilities x)
      guard (makeSolution s' `mightSatisfy` prob)
      State.put s'

mightSatisfy :: Solution -> Problem -> Bool
mightSatisfy sol (Problem constraints) =
  all (sol `mightSatisfyConstraint`) constraints

mightSatisfyConstraint :: Solution -> Disjunction -> Bool
mightSatisfyConstraint (Solution sol) (Disjunction assigns) = any
  (not . disagrees)
  assigns
 where
  disagrees :: Assignment -> Bool
  disagrees (Assignment var val) = Map.lookup var sol == Just (not val)
