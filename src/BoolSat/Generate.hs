module BoolSat.Generate
  ( ProblemGenerator(..)
  , makeInstance
  )
where

import           BoolSat.Prelude

import           BoolSat.Data
import           Control.Monad                  ( replicateM )
import           Control.Monad.Random           ( MonadRandom
                                                , getRandomR
                                                )
import qualified Data.Set                      as Set

data ProblemGenerator m = ProblemGenerator
  { genNumVars :: m Int
  , genNumConstraints :: Int -> m Int -- numVars -> numConstraints
  , genConstraintLength :: m Int
  }

makeInstance :: MonadRandom m => ProblemGenerator m -> m Problem
makeInstance ProblemGenerator {..} = do
  numVars        <- genNumVars
  numConstraints <- genNumConstraints numVars
  fmap Problem $ replicateM numConstraints $ do
    clen <- genConstraintLength
    Disjunction . Set.fromList <$> replicateM
      clen
      (Assignment . Variable <$> getRandomR (1, numVars) <*> getRandomR
        (sfalse, strue)
      )
