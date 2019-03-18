{-# LANGUAGE UndecidableInstances #-}

module BoolSat.Solver.CDCL.Monad.Mask
  ( MonadMaskBase(..)
  )
where

import           DSpies.Prelude

import           Control.Monad.Trans.Control

import           Control.Monad.Yield.ST

class Monad m => MonadMaskBase m where
  type Base m :: * -> *
  finally :: m a -> Base m () -> m a

instance MonadMaskBase (YieldST s a) where
  type Base (YieldST s a) = YieldST s a
  finally act after = do
    result <- act
    after
    return result

instance (MonadTransControl t, MonadMaskBase m, Monad (t m))
    => MonadMaskBase (Transformed t m) where
  type Base (Transformed t m) = Base m
  finally act after = liftWithCarry $ \run -> finally (run act) after
deriving via (Transformed (ReaderT r) m) instance MonadMaskBase m
  => MonadMaskBase (ReaderT r m)
deriving via (Transformed (ExceptT e) m) instance MonadMaskBase m
  => MonadMaskBase (ExceptT e m)
deriving via (Transformed (StateT s') m) instance MonadMaskBase m
  => MonadMaskBase (StateT s' m)
deriving via (Transformed (WriterT w) m) instance (MonadMaskBase m, Monoid w)
  => MonadMaskBase (WriterT w m)
