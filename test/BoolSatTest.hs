{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module BoolSatTest where


import           DSpies.Prelude

import           Control.DeepSeq                ( deepseq )
import           Control.Monad.Random           ( MonadRandom(..) )
import           Data.Reflection                ( Reifies
                                                , reify
                                                , reflect
                                                )
import qualified Data.Set                      as Set
import           Test.Hspec
import           Test.QuickCheck

import           BoolSat.Data
import           BoolSat.Generate
import           BoolSat.Solver.CDCL            ( CDCL(CDCL) )
import           BoolSat.Solver.DPLL            ( DPLL(DPLL) )
import           BoolSat.Solver.Naive           ( Naive(Naive) )
import           BoolSat.Solver.Partial         ( Partial(Partial) )
import           BoolSat.SomeSolver             ( SomeSolver(Some) )

newtype ProblemFrom gen = ProblemFrom Problem
  deriving newtype Show

newtype RandGen a = RandGen {unRandGen :: Gen a}
  deriving (Functor, Applicative, Monad)

instance MonadRandom RandGen where
  getRandomR  = RandGen . choose
  getRandom   = undefined
  getRandomRs = undefined
  getRandoms  = undefined

instance Reifies gen (ProblemGenerator RandGen)
  => Arbitrary (ProblemFrom gen) where
  arbitrary =
    unRandGen $ ProblemFrom <$> makeInstance (reflect (Proxy :: Proxy gen))
  shrink (ProblemFrom (Problem ds)) =
    ProblemFrom
      .   Problem
      <$> shrinkList
            (\(Disjunction s) -> Disjunction . Set.fromList <$> shrinkList
              shrinkNothing
              (Set.toList s)
            )
            ds

handlesProblem
  :: Reifies gen (ProblemGenerator RandGen)
  => (Problem -> Property)
  -> proxy gen
  -> ProblemFrom gen
  -> Property
handlesProblem f _ (ProblemFrom prob) = within 1000000 $ f prob

propSatisfies
  :: (Reifies gen (ProblemGenerator RandGen), Solver solver)
  => solver
  -> proxy gen
  -> ProblemFrom gen
  -> Property
propSatisfies solver = handlesProblem $ \prob -> conjoin
  [ sol `shouldSatisfy` (`satisfies` prob) | sol <- solver `solve` prob ]

propAgrees
  :: (Reifies gen (ProblemGenerator RandGen), Solver solver1, Solver solver2)
  => solver1
  -> solver2
  -> proxy gen
  -> ProblemFrom gen
  -> Property
propAgrees solver1 solver2 = handlesProblem $ \prob ->
  ioProperty
    $               (solver1 `solve` prob, solver2 `solve` prob)
    `shouldSatisfy` agreesOnNullity

agreesOnNullity :: ([a], [a]) -> Bool
agreesOnNullity = \case
  ([]   , []   ) -> True
  (_ : _, []   ) -> False
  ([]   , _ : _) -> False
  (_ : _, _ : _) -> True

smallProblemGen :: Applicative m => ProblemGenerator m
smallProblemGen = ProblemGenerator { genNumVars          = pure 10
                                   , genNumConstraints   = const $ pure 43
                                   , genConstraintLength = pure 3
                                   }

solvers :: [SomeSolver]
solvers = [Some Partial, Some DPLL, Some CDCL]

spec_solve :: Spec
spec_solve = do
  describe "Naive"
    $ it "should return solutions which satisfy all constraints"
    $ reify smallProblemGen
    $ property
    . propSatisfies Naive
  describe "CDCL" $ it
    "should handle that one case"
    ((CDCL `solve` uglyCase) `shouldSatisfy` (`deepseq` True))
  forM_ solvers $ \(Some solver) -> describe (show solver) $ do
    it "should return solutions which satisfy all constraints"
      $ reify smallProblemGen
      $ property
      . propSatisfies solver
    it "should agree with Naive"
      $ reify smallProblemGen
      $ property
      . propAgrees solver Naive

disj :: [Int] -> Disjunction
disj = Disjunction . Set.fromList . map intToAssign

uglyCase :: Problem
uglyCase = Problem
  [ disj [5, -7]
  , disj [-2, 10]
  , disj [1, -3, -4]
  , disj [3, 10]
  , disj [-2, -10]
  , disj [3, 5]
  , disj [-1, -5]
  , disj [-1, 2]
  , disj [1, -3, 4]
  ]
