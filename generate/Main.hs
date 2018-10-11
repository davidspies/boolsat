module Main where

import           BoolSat.Prelude

import           System.Environment             ( getArgs )
import qualified Data.Set                      as Set

import           BoolSat.Data
import           BoolSat.Generate

main :: IO ()
main = do
  [nvars, nclauses] <- getArgs
  let genNumVars          = pure $ read nvars
      genNumConstraints   = const $ pure $ read nclauses
      genConstraintLength = pure 3
  putStrLn . exportProblem =<< makeInstance ProblemGenerator {..}

exportProblem :: Problem -> String
exportProblem (Problem disjs) =
  unlines
    $ unwords ["p", "cnf", show nvars, show nclauses]
    : [ unwords $ map show ([ assignToInt a | a <- Set.toList d ] ++ [0])
      | Disjunction d <- disjs
      ]
 where
  nclauses = length disjs
  nvars    = maximum
    [ v | Disjunction d <- disjs, Assignment (Variable v) _ <- Set.toList d ]
