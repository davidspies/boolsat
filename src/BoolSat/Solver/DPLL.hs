module BoolSat.Solver.DPLL
  ( DPLL(..)
  )
where

import           BoolSat.Prelude

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           BoolSat.Data
import qualified BoolSat.Data.ListMap          as ListMap

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
    Nothing -> [ sol | sol `satisfies` prob ]
    Just v  -> do
      a <- [ Assignment v b | b <- [sfalse, strue] ]
      go =<< inference (addAssignment a sol)

addAssignment :: Assignment -> Solution -> Solution
addAssignment (Assignment v b) (Solution sol) = Solution $ Map.insert v b sol

makeNextChoice :: Set Variable -> Solution -> Maybe Variable
makeNextChoice vs (Solution sol) = find (`Map.notMember` sol) vs

emptySol :: Solution
emptySol = makeSolution []

contextOf :: Problem -> Context
contextOf prob@(Problem disjs) = Context
  (ListMap.build $ do
    d@(Disjunction assigns) <- disjs
    a                       <- Set.toList assigns
    return (a, d)
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

addAllAssignments :: Solution -> [Assignment] -> Solution
addAllAssignments (Solution current) newAssigns = Solution
  $ Map.union current (Map.fromList [ (v, s) | Assignment v s <- newAssigns ])

unassignedVars :: Solution -> Problem -> [Variable]
unassignedVars (Solution solAssigns) =
  filter (`Map.notMember` solAssigns) . Set.toList . allVars

unitPropogation, pureLiteralElimination, allInferenceSteps :: Inference
unitPropogation (Context vm (Problem disjs)) = go disjs
 where
  go ds sol@(Solution solAssigns) = case newAssigns of
    Nothing         -> Nothing
    Just []         -> Just sol
    Just na@(_ : _) -> go
      (do
        Assignment var val <- na
        vm ListMap.! Assignment var (opp val)
      )
      (addAllAssignments sol na)
   where
    inverted :: Assignment -> Bool
    inverted (Assignment v b) = Map.lookup v solAssigns == Just (opp b)
    newAssigns :: Maybe [Assignment]
    newAssigns = joinResults $ (`map` ds) $ \(Disjunction d) ->
      case filter (not . inverted) (Set.toList d) of
        []      -> Unsat
        [a@(Assignment v _)] | v `Map.notMember` solAssigns -> Assign a
        (_ : _) -> NoLearn

pureLiteralElimination (Context vm prob) sol =
  Just $ addAllAssignments sol $ mapMaybe pureValue (unassignedVars sol prob)
 where
  occursWithSign :: Variable -> Sign -> Bool
  occursWithSign var val =
    any (not . satisfiesConstraint sol) (vm ListMap.! Assignment var val)
  pureValue :: Variable -> Maybe Assignment
  pureValue var | not (occursWithSign var strue)  = Just (Assignment var sfalse)
                | not (occursWithSign var sfalse) = Just (Assignment var strue)
                | otherwise                       = Nothing

allInferenceSteps prob = pureLiteralElimination prob <=< unitPropogation prob
