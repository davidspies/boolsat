module Main where

import           BoolSat.Data
import           BoolSat.Solver.Partial         ( Partial(Partial) )
import           Dimacs.Parse                   ( parse )

main :: IO ()
main = interact (showSolutions . solve Partial . parse)

showSolutions :: [Solution ()] -> String
showSolutions = unlines . \case
  [] -> ["s UNSATISFIABLE"]
  sols@(_ : _) ->
    "s SATISFIABLE" : [ unwords ("v" : map show (asInts res)) | res <- sols ]
