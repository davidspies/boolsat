module Main where

import           BoolSat.Data
import           BoolSat.Solver.DPLL            ( DPLL(DPLL) )
import           Dimacs.Parse                   ( parse )

main :: IO ()
main = interact (showSolutions . solve DPLL . parse)

showSolutions :: [Solution ()] -> String
showSolutions = unlines . \case
  [] -> ["s UNSATISFIABLE"]
  sols@(_ : _) ->
    "s SATISFIABLE" : [ unwords ("v" : map show (asInts res)) | res <- sols ]
