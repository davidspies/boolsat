module BoolSat.Data where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

newtype Problem = Problem [Disjunction]
  deriving (Show)
newtype Disjunction = Disjunction (Set Assignment)
  deriving (Show)
newtype Variable = Variable Int
  deriving (Eq, Ord, Show)
data Assignment = Assignment Variable Bool
  deriving (Eq, Ord, Show)
newtype Solution = Solution (Map Variable Bool)
  deriving (Eq, Ord)

problemToCoerced :: Problem -> [Set (Int, Bool)]
problemToCoerced (Problem ds) = map
  (\(Disjunction s) -> Set.map (\(Assignment (Variable v) b) -> (v, b)) s)
  ds

coercedToProblem :: [Set (Int, Bool)] -> Problem
coercedToProblem =
  Problem . map (Disjunction . Set.map (\(v, b) -> Assignment (Variable v) b))

instance Show Solution where
  showsPrec d s =
    showParen (d > 10) $ showString "solInts " . showsPrec d (asInts s)

class Solver s where
  solve :: s -> Problem -> [Solution]

satisfies :: Solution -> Problem -> Bool
satisfies sol (Problem constraints) =
  all (sol `satisfiesConstraint`) constraints

satisfiesConstraint :: Solution -> Disjunction -> Bool
satisfiesConstraint (Solution sol) (Disjunction assigns) = any matched assigns
 where
  matched :: Assignment -> Bool
  matched (Assignment var val) = Map.lookup var sol == Just val

allVars :: Problem -> Set Variable
allVars (Problem djs) = Set.unions (map varsOf djs)
 where
  varsOf :: Disjunction -> Set Variable
  varsOf (Disjunction assgns) =
    Set.fromList $ map (\(Assignment v _) -> v) $ Set.toList assgns

possibilities :: Variable -> [Assignment]
possibilities v = map (Assignment v) [False, True]

makeSolution :: [Assignment] -> Solution
makeSolution = Solution . Map.fromList . map assgnToPr
  where assgnToPr (Assignment v a) = (v, a)

solInts :: [Int] -> Solution
solInts = makeSolution . map intToAssign

intToAssign :: Int -> Assignment
intToAssign n | n < 0     = Assignment (Variable (-n)) False
              | n > 0     = Assignment (Variable n) True
              | otherwise = error "intToAssign 0"

assignToInt :: Assignment -> Int
assignToInt (Assignment (Variable n) b) | b         = n
                                        | otherwise = -n

asInts :: Solution -> [Int]
asInts (Solution m) = map assignInt $ Map.toList m
  where assignInt (Variable v, b) = if b then v else (-v)
