{-# LANGUAGE UndecidableInstances #-}

module BoolSat.Solver.CDCL.LevelErrors
  ( LevelErrorsT
  , Level
  , Levelable(..)
  , MonadHasLevels
  , MonadReadLevel(..)
  , MonadThrowLevel(..)
  , level0
  , onNextLevel
  , runLevelErrorsT
  )
where

import           DSpies.Prelude          hiding ( throwError )

import           Control.Monad.Except           ( ExceptT(ExceptT) )
import qualified Control.Monad.Except          as Except
import           Control.Monad.Reader           ( ReaderT(ReaderT) )
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Trans.Control    ( MonadTransControl )
import qualified Control.Monad.Trans.Control   as TC

import           BoolSat.Solver.CDCL.Yield      ( MonadYield )

newtype Level = Level Int
  deriving (Eq, Ord)
  deriving newtype Show

class Levelable err where
  level :: err -> Level

level0 :: Level
level0 = Level 0

incrLevel :: Level -> Level
incrLevel (Level lev) = Level (lev + 1)

class Monad m => MonadReadLevel m where
  askLevel :: m Level
instance (MonadTrans t, MonadReadLevel m, Monad (t m))
    => MonadReadLevel (Transformed t m) where
  askLevel = lift askLevel
deriving via Transformed (StateT s) m instance MonadReadLevel m
  => MonadReadLevel (StateT s m)
deriving via Transformed (ExceptT e) m instance MonadReadLevel m
  => MonadReadLevel (ExceptT e m)
instance Monad m => MonadReadLevel (ReaderT Level m) where
  askLevel = Reader.ask

class MonadReadLevel m => MonadThrowLevel err m | m -> err where
  throwError :: err -> m a
instance (Monad m, MonadTrans t, Monad (t m), MonadThrowLevel err m)
    => MonadThrowLevel err (Transformed t m) where
  throwError = lift . throwError
deriving via Transformed (StateT s) m instance MonadThrowLevel err m
  => MonadThrowLevel err (StateT s m)
instance MonadReadLevel m => MonadThrowLevel err (ExceptT err m) where
  throwError = Except.throwError

class (Levelable err, MonadReadLevel m, MonadThrowLevel err m)
    => MonadHasLevels err m | m -> err where
  localLevel :: (Level -> Level) -> m a -> m a
  catchError :: m a -> (err -> m a) -> m a
instance (Monad m, MonadTransControl t, Monad (t m), MonadHasLevels err m)
    => MonadHasLevels err (Transformed t m) where
  localLevel op act = liftWithCarry $ \run -> localLevel op (run act)
  catchError act onError =
    liftWithCarry $ \run -> run act `catchError` (run . onError)
deriving via Transformed (StateT s) m instance MonadHasLevels err m
  => MonadHasLevels err (StateT s m)
instance (Levelable err, Monad m)
    => MonadHasLevels err (ExceptT err (ReaderT Level m)) where
  localLevel = Reader.local
  catchError = Except.catchError

onNextLevel :: (MonadHasLevels err m) => m b -> m (Either err b)
onNextLevel act = localLevel incrLevel $ (Right <$> act) `catchError` \err ->
  do
    current <- askLevel
    if level err > current then return (Left err) else throwError err

newtype LevelErrorsT err m a
    = LevelErrorsT {unLevelErrorsT :: ExceptT err (ReaderT Level m) a}
  deriving ( Functor, Applicative, Monad, MonadState s
           , MonadReadLevel, MonadThrowLevel err, MonadHasLevels err )
instance MonadTrans (LevelErrorsT err) where
  lift = LevelErrorsT . lift . lift
instance MonadTransControl (LevelErrorsT err) where
  type StT (LevelErrorsT err) a
    = TC.StT (ExceptT err) (TC.StT (ReaderT Level) a)
  liftWith act = LevelErrorsT $ TC.liftWith $ \runE ->
    TC.liftWith $ \runR -> act (runR . runE . unLevelErrorsT)
  restoreT = LevelErrorsT . TC.restoreT . TC.restoreT
deriving via Transformed (LevelErrorsT err) m instance MonadYield y m
  => MonadYield y (LevelErrorsT err m)

runLevelErrorsT :: Monad m => LevelErrorsT err m a -> m (Either err a)
runLevelErrorsT = (`runReaderT` level0) . runExceptT . unLevelErrorsT
