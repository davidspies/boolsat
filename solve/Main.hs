module Main where

import           DSpies.Prelude

import           System.Environment             ( getArgs )

import           BoolSat.Data
import           BoolSat.Dimacs.Parse           ( parse )
import           BoolSat.Solver.DPLL            ( DPLL(DPLL) )
import           BoolSat.SomeSolver             ( SomeSolver(Some) )

main :: IO ()
main = do
  Some solver <- getArgs <&> \case
    []        -> Some DPLL
    [     s ] -> read s
    _ : _ : _ -> error "Too many arguments"
  interact (showSolutions . solve solver . parse)

showSolutions :: [Solution] -> String
showSolutions = unlines . \case
  [] -> ["s UNSATISFIABLE"]
  sols@(_ : _) ->
    "s SATISFIABLE" : [ unwords ("v" : map show (asInts res)) | res <- sols ]
