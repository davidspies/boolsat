module BoolSat.Data where

import           DSpies.Prelude

import           Control.DeepSeq                ( NFData )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           System.Random                  ( Random )

newtype Problem = Problem [Disjunction]
instance Show Problem where
  showsPrec d xs =
    showParen (d > 10) $ showString "problem " . showsPrec 11 (probToInts xs)
newtype Disjunction = Disjunction (Set Assignment)
instance Show Disjunction where
  showsPrec d xs =
    showParen (d > 10) $ showString "disj " . showsPrec 11 (disjToInts xs)
newtype Variable = Variable Int
  deriving (Eq, Ord, Show, NFData)
newtype Sign = Sign Bool
  deriving newtype (Eq, Ord, Random, NFData)
instance Show Sign where
  show (Sign b) = if b then "strue" else "sfalse"
data Assignment = Assignment Variable Sign
  deriving (Eq, Ord)
instance Show Assignment where
  showsPrec d a =
    showParen (d > 10) $ showString "assign " . showsPrec 11 (assignToInt a)
newtype Solution = Solution (Map Variable Sign)
  deriving (Eq, Ord, Semigroup, Monoid, Generic)
instance NFData Solution

sfalse, strue :: Sign
sfalse = Sign False
strue = Sign True

opp :: Sign -> Sign
opp (Sign x) = Sign (not x)

problemToCoerced :: Problem -> [Set (Int, Sign)]
problemToCoerced (Problem ds) = map
  (\(Disjunction s) -> Set.map (\(Assignment (Variable v) b) -> (v, b)) s)
  ds

coercedToProblem :: [Set (Int, Sign)] -> Problem
coercedToProblem =
  Problem . map (Disjunction . Set.map (\(v, b) -> Assignment (Variable v) b))

instance Show Solution where
  showsPrec d s =
    showParen (d > 10) $ showString "solution " . showsPrec d (solToInts s)

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
possibilities v = map (Assignment v) [sfalse, strue]

makeSolution :: [Assignment] -> Solution
makeSolution = Solution . Map.fromList . map assgnToPr
  where assgnToPr (Assignment v a) = (v, a)

solution :: [Int] -> Solution
solution = makeSolution . map assign

assign :: Int -> Assignment
assign n | n < 0     = Assignment (Variable (-n)) sfalse
         | n > 0     = Assignment (Variable n) strue
         | otherwise = error "assign 0"

assignToInt :: Assignment -> Int
assignToInt (Assignment (Variable n) (Sign b)) | b         = n
                                               | otherwise = -n

solToInts :: Solution -> [Int]
solToInts (Solution m) = map (assignToInt . uncurry Assignment) $ Map.toList m

disj :: [Int] -> Disjunction
disj = Disjunction . Set.fromList . map assign

disjToInts :: Disjunction -> [Int]
disjToInts (Disjunction xs) = map assignToInt $ Set.toList xs

problem :: [[Int]] -> Problem
problem = Problem . map disj

probToInts :: Problem -> [[Int]]
probToInts (Problem xs) = map disjToInts xs
