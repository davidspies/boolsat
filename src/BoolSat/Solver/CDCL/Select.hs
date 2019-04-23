module BoolSat.Solver.CDCL.Select
  ( MonadSelect(..)
  )
where

import           DSpies.Prelude

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

class MonadSelect m where
  selectLiteral :: m Assignment

instance  MonadSelect CDCLM where
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
