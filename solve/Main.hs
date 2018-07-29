module Main where

import           BoolSat.Data
import           BoolSat.Solver.DPLL            ( DPLL(DPLL) )
import           BoolSat.SomeSolver             ( SomeSolver(Some) )
import           Dimacs.Parse                   ( parse )
import           System.Environment             ( getArgs )

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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
