{-# OPTIONS_GHC -Wno-unused-matches #-}

module BoolSat.Solver.DPLL
  ( DPLL(..)
  )
where

import           Control.Monad
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Map                      as Map
import           Data.Foldable                  ( find )

import           BoolSat.Data

data DPLL = DPLL
  deriving (Read, Show)

instance Solver DPLL where
  solve DPLL = solution

solution :: Problem -> [Solution]
solution prob = go =<< inference emptySol
 where
  vars :: Set Variable
  vars = allVars prob
  inference :: Solution -> [Solution]
  inference = maybeToList . allInferenceSteps prob
  go :: Solution -> [Solution]
  go sol = case makeNextChoice vars sol of
    Nothing -> do
      guard $ sol `satisfies` prob
      [sol]
    Just v -> do
      a <- [ Assignment v b | b <- [False, True] ]
      go =<< inference (addAssignment a sol)

addAssignment :: Assignment -> Solution -> Solution
addAssignment (Assignment v b) (Solution sol) =
  Solution $ Map.insert v b sol

makeNextChoice :: Set Variable -> Solution -> Maybe Variable
makeNextChoice vs (Solution sol) = find (`Map.notMember` sol) vs

emptySol :: Solution
emptySol = makeSolution []

type Inference = Problem -> Solution -> Maybe Solution

data UnitResult = NoLearn | Unsat | Assign Assignment

joinResults :: [UnitResult] -> Maybe [Assignment]
joinResults = \case
  []              -> Just []
  (NoLearn  : xs) -> joinResults xs
  (Unsat    : _ ) -> Nothing
  (Assign a : xs) -> (a :) <$> joinResults xs

unitPropogation, pureLiteralElimination, allInferenceSteps :: Inference
unitPropogation (Problem disjs) = go
 where
  go sol@(Solution solAssigns) = case newAssigns of
    Nothing         -> Nothing
    Just []         -> Just sol
    Just na@(_ : _) -> go (foldl (flip addAssignment) sol na)
   where
    inverted :: Assignment -> Bool
    inverted (Assignment v b) =
      Map.lookup v solAssigns == Just (not b)
    newAssigns :: Maybe [Assignment]
    newAssigns = joinResults $ (`map` disjs) $ \(Disjunction d) ->
      case filter (not . inverted) (Set.toList d) of
        []      -> Unsat
        [a@(Assignment v _)] | v `Map.notMember` solAssigns -> Assign a
        (_ : _) -> NoLearn

pureLiteralElimination prob@(Problem disjs) sol =
  Just $ foldl (flip addAssignment) sol $ mapMaybe pureValue
                                                   (Set.toList $ allVars prob)
 where
  occursWithSign :: Variable -> Bool -> Bool
  occursWithSign var val =
    any (\(Disjunction bs) -> Assignment var val `Set.member` bs) disjs
  pureValue :: Variable -> Maybe Assignment
  pureValue var
    | not (occursWithSign var True)  = Just (Assignment var False)
    | not (occursWithSign var False) = Just (Assignment var True)
    | otherwise                      = Nothing

allInferenceSteps prob = pureLiteralElimination prob <=< unitPropogation prob
