{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad                  ( replicateM )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Reflection
import qualified Data.Set                      as Set
import           Test.Hspec
import           Test.QuickCheck
import           Data.List                      ( sort )

import           BoolSat.Data
import           BoolSat.Solver.Naive
import           BoolSat.Solver.Partial

main :: IO ()
main = hspec spec

data ProblemGenerator = ProblemGenerator
  { genNumVars :: Gen Int
  , genNumConstraints :: Int -> Gen Int -- numVars -> numConstraints
  , genConstraintLength :: Gen Int
  }

newtype ProblemFrom gen = ProblemFrom Problem
  deriving newtype Show

instance Reifies gen ProblemGenerator => Arbitrary (ProblemFrom gen) where
  arbitrary = do
    let ProblemGenerator{..} = reflect (Proxy :: Proxy gen)
    numVars <- genNumVars
    numConstraints <- genNumConstraints numVars
    fmap (ProblemFrom . Problem) $ replicateM numConstraints $ do
      clen <- genConstraintLength
      fmap (Disjunction . Set.fromList) $ replicateM clen $
        Assignment <$> (Variable <$> choose (1, numVars)) <*> arbitrary
  shrink (ProblemFrom (Problem ds)) = ProblemFrom . Problem <$>
    shrinkList
      (\(Disjunction s) ->
        Disjunction . Set.fromList <$> shrinkList shrinkNothing (Set.toList s)
      )
      ds

handlesProblem
  :: Reifies gen ProblemGenerator
  => (Problem -> Property)
  -> proxy gen
  -> ProblemFrom gen
  -> Property
handlesProblem f _ (ProblemFrom prob) = f prob

prop_satisfies
  :: (Reifies gen ProblemGenerator, Solver solver)
  => solver
  -> proxy gen
  -> ProblemFrom gen
  -> Property
prop_satisfies solver = handlesProblem
  (\prob -> conjoin $ map (`satisfies` prob) $ solver `solve` prob)

prop_agrees
  :: (Reifies gen ProblemGenerator, Solver solver1, Solver solver2)
  => solver1
  -> solver2
  -> proxy gen
  -> ProblemFrom gen
  -> Property
prop_agrees solver1 solver2 = handlesProblem
  (\prob -> sort (solver1 `solve` prob) === sort (solver2 `solve` prob))

smallProblemGen :: ProblemGenerator
smallProblemGen = ProblemGenerator
  { genNumVars          = pure 10
  , genNumConstraints   = const $ pure 43
  , genConstraintLength = pure 3
  }

spec :: Spec
spec = do
  describe "Naive"
    $ it "should return solutions which satisfy all constraints"
    $ reify smallProblemGen
    $ property
    . prop_satisfies Naive
  describe "Partial" $ do
    it "should return solutions which satisfy all constraints"
      $ reify smallProblemGen
      $ property
      . prop_satisfies Partial
    it "should agree with Naive"
      $ reify smallProblemGen
      $ property
      . prop_agrees Partial Naive
