module BoolSat.Solver.CDCL.Yield
  ( MonadYield(..)
  , YieldM
  , execYieldM
  )
where

import           DSpies.Prelude

class Monad m => MonadYield y m where
  yield :: y -> m ()
instance (MonadYield y m, MonadTrans t, Monad (t m))
    => MonadYield y (Transformed t m) where
  yield = lift . yield
deriving via Transformed (StateT s) m instance MonadYield y m
    => MonadYield y (StateT s m)

data YieldM y a = Final a | NextResult y (YieldM y a)
  deriving (Functor)
instance Applicative (YieldM y) where
  pure  = Final
  (<*>) = ap
instance Monad (YieldM y) where
  (>>=) act fn = case act of
    Final x        -> fn x
    NextResult a r -> NextResult a (r >>= fn)
instance MonadYield y (YieldM y) where
  yield y = NextResult y (return ())

execYieldM :: YieldM y a -> [y]
execYieldM = \case
  Final _        -> []
  NextResult x r -> x : execYieldM r
