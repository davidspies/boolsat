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

instance  MonadSelect (CDCLM s) where
  selectLiteral = do
    VarState m                          <- getAssignment
    RuleSet { original = Problem orig } <- getRules
    return $ head $ concatMap
      (\(Disjunction rs) ->
        mapMaybe
            (\(Assignment var _) -> case m Map.! var of
              Unassigned _ -> Just (Assignment var sfalse)
              Assigned   _ -> Nothing
            )
          $ Set.toList rs
      )
      orig
