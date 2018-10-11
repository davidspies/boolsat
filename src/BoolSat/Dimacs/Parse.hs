module BoolSat.Dimacs.Parse
  ( parse
  )
where

import           BoolSat.Prelude

import           Control.Monad.State            ( State
                                                , evalState
                                                )
import qualified Control.Monad.State           as State
import           Data.List                      ( head )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as Set

import           BoolSat.Data

startswith :: Eq a => [a] -> [a] -> Bool
startswith xs pref = take (length pref) xs == pref

tryConsumeTerm :: State [a] (Maybe a)
tryConsumeTerm = State.get >>= \case
  []       -> return Nothing
  (x : xs) -> State.put xs >> return (Just x)

consumeTerm :: State [a] a
consumeTerm = fromJust <$> tryConsumeTerm

consumeAll :: State [a] [a]
consumeAll = do
  res <- State.get
  State.put []
  return res

pNext :: (a -> Bool) -> State [a] Bool
pNext p = State.gets (p . head)

consumeComments :: State [String] ()
consumeComments = pNext (`startswith` "c") >>= \case
  True  -> consumeTerm >> consumeComments
  False -> return ()

countLine :: State [String] (Int, Int)
countLine = parseCounts <$> consumeTerm

parseCounts :: String -> (Int, Int)
parseCounts s = case words s of
  ["p", "cnf", snvars, snclauses] -> (read snvars, read snclauses)
  _ -> error $ "Cannot parse count line: " ++ show s

allInts :: [String] -> [Int]
allInts = map read . words . unlines

readClause :: State [Int] Disjunction
readClause = Disjunction . Set.fromList <$> go
 where
  go :: State [Int] [Assignment]
  go = tryConsumeTerm >>= \case
    Nothing -> return []
    Just 0  -> return []
    Just i  -> (intToAssign i :) <$> go

problemReader :: State [String] Problem
problemReader = do
  consumeComments
  (_nvars, nclauses) <- countLine
  Problem . evalState (replicateM nclauses readClause) . allInts <$> consumeAll

parse :: String -> Problem
parse = evalState problemReader . lines
