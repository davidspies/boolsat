module BoolSat.Solver.CDCL.Select
  ( MonadSelect(..)
  )
where

import           DSpies.Prelude

import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad

class MonadSelect m where
  selectLiteral :: m Assignment

instance  MonadSelect CDCL where
  selectLiteral = do
    orig <- getBaseClauses
    fromJust
      <$> firstJustM
            (\(Disjunction rs) -> firstJustM
              (\(Assignment k _) -> isAssigned k <&> \case
                False -> Just $ Assignment k sfalse
                True  -> Nothing
              )
              (Set.toList rs)
            )
            orig
