module BoolSat.Solver.DPLL
  ( DPLL(..)
  )
where

import           Control.Monad
import qualified Data.DList                    as DList
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Foldable                  ( find )

import           BoolSat.Data

data DPLL = DPLL
  deriving (Read, Show)

instance Solver DPLL where
  solve DPLL = solution

data Context = Context (Map Assignment [Disjunction]) Problem

solution :: Problem -> [Solution]
solution prob = go =<< inference emptySol
 where
  vars :: Set Variable
  vars = allVars prob
  ctx  = contextOf prob
  inference :: Solution -> [Solution]
  inference = maybeToList . allInferenceSteps ctx
  go :: Solution -> [Solution]
  go sol = case makeNextChoice vars sol of
    Nothing -> do
      guard $ sol `satisfies` prob
      [sol]
    Just v -> do
      a <- [ Assignment v b | b <- [False, True] ]
      go =<< inference (addAssignment a sol)

addAssignment :: Assignment -> Solution -> Solution
addAssignment (Assignment v b) (Solution sol) = Solution $ Map.insert v b sol

makeNextChoice :: Set Variable -> Solution -> Maybe Variable
makeNextChoice vs (Solution sol) = find (`Map.notMember` sol) vs

emptySol :: Solution
emptySol = makeSolution []

contextOf :: Problem -> Context
contextOf prob@(Problem disjs) = Context
  (Map.map DList.toList $ Map.fromListWith
    (<>)
    [ (a, DList.singleton d)
    | d@(Disjunction assigns) <- disjs
    , a                       <- Set.toList assigns
    ]
  )
  prob

type Inference = Context -> Solution -> Maybe Solution

data UnitResult = NoLearn | Unsat | Assign Assignment

joinResults :: [UnitResult] -> Maybe [Assignment]
joinResults = \case
  []              -> Just []
  (NoLearn  : xs) -> joinResults xs
  (Unsat    : _ ) -> Nothing
  (Assign a : xs) -> (a :) <$> joinResults xs

unitPropogation, pureLiteralElimination, allInferenceSteps :: Inference
unitPropogation (Context vm (Problem disjs)) = go disjs
 where
  go ds sol@(Solution solAssigns) = case newAssigns of
    Nothing         -> Nothing
    Just []         -> Just sol
    Just na@(_ : _) -> go
      (concatMap
        (\(Assignment var val) ->
          fromMaybe [] $ vm Map.!? Assignment var (not val)
        )
        na
      )
      (foldl (flip addAssignment) sol na)
   where
    inverted :: Assignment -> Bool
    inverted (Assignment v b) = Map.lookup v solAssigns == Just (not b)
    newAssigns :: Maybe [Assignment]
    newAssigns = joinResults $ (`map` ds) $ \(Disjunction d) ->
      case filter (not . inverted) (Set.toList d) of
        []      -> Unsat
        [a@(Assignment v _)] | v `Map.notMember` solAssigns -> Assign a
        (_ : _) -> NoLearn

pureLiteralElimination (Context vm prob) sol@(Solution solAssigns) =
  Just $ foldl (flip addAssignment) sol $ mapMaybe
    pureValue
    (filter (`Map.notMember` solAssigns) $ Set.toList $ allVars prob)
 where
  occursWithSign :: Variable -> Bool -> Bool
  occursWithSign var val = any (not . satisfiesConstraint sol)
                               (fromMaybe [] $ vm Map.!? Assignment var val)
  pureValue :: Variable -> Maybe Assignment
  pureValue var | not (occursWithSign var True)  = Just (Assignment var False)
                | not (occursWithSign var False) = Just (Assignment var True)
                | otherwise                      = Nothing

allInferenceSteps prob = pureLiteralElimination prob <=< unitPropogation prob
