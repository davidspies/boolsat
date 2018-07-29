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

type Level = Int

instance Solver DPLL where
  solve DPLL = solution

solution :: Problem -> [Solution ()]
solution prob = map void $ go 1 =<< inference 0 emptySol
 where
  vars :: Set Variable
  vars = allVars prob
  inference :: Level -> Solution Level -> [Solution Level]
  inference lvl = maybeToList . allInferenceSteps lvl prob
  go :: Level -> Solution Level -> [Solution Level]
  go level sol = case makeNextChoice vars sol of
    Nothing -> do
      guard $ sol `satisfies` prob
      [sol]
    Just v -> do
      a <- [ (Assignment v b, level) | b <- [False, True] ]
      go (level + 1) =<< inference level (addAssignment a sol)

addAssignment :: (Assignment, a) -> Solution a -> Solution a
addAssignment (Assignment v b, x) (Solution sol) =
  Solution $ Map.insert v (b, x) sol

makeNextChoice :: Set Variable -> Solution a -> Maybe Variable
makeNextChoice vs (Solution sol) = find (`Map.notMember` sol) vs

emptySol :: Solution a
emptySol = makeSolution' []

type Inference = Level -> Problem -> Solution Level -> Maybe (Solution Level)

data UnitResult = NoLearn | Unsat | Assign Assignment

joinResults :: [UnitResult] -> Maybe [Assignment]
joinResults = \case
  []              -> Just []
  (NoLearn  : xs) -> joinResults xs
  (Unsat    : _ ) -> Nothing
  (Assign a : xs) -> (a :) <$> joinResults xs

unitPropogation, pureLiteralElimination, allInferenceSteps :: Inference
unitPropogation level (Problem disjs) = go
 where
  go sol@(Solution solAssigns) = case newAssigns of
    Nothing         -> Nothing
    Just []         -> Just sol
    Just na@(_ : _) -> go (foldl (flip addAssignment) sol na)
   where
    inverted :: Assignment -> Bool
    inverted (Assignment v b) =
      (fst <$> Map.lookup v solAssigns) == Just (not b)
    newAssigns :: Maybe [(Assignment, Level)]
    newAssigns =
      fmap (map (, level)) $ joinResults $ (`map` disjs) $ \(Disjunction d) ->
        case filter (not . inverted) (Set.toList d) of
          []      -> Unsat
          [a@(Assignment v _)] | v `Map.notMember` solAssigns -> Assign a
          (_ : _) -> NoLearn

pureLiteralElimination level prob@(Problem disjs) sol =
  Just $ foldl (flip addAssignment) sol $ mapMaybe pureValue
                                                   (Set.toList $ allVars prob)
 where
  occursWithSign :: Variable -> Bool -> Bool
  occursWithSign var val =
    any (\(Disjunction bs) -> Assignment var val `Set.member` bs) disjs
  pureValue :: Variable -> Maybe (Assignment, Level)
  pureValue var
    | not (occursWithSign var True)  = Just (Assignment var False, level)
    | not (occursWithSign var False) = Just (Assignment var True, level)
    | otherwise                      = Nothing

allInferenceSteps level prob =
  pureLiteralElimination level prob <=< unitPropogation level prob
