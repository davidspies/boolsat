module BoolSat.Solver.CDCL.Monad.Internal
  ( AssignInfo(..)
  , CDCL(..)
  , CDCLState(..)
  , Conflict(..)
  , Level
  , decrLevel
  , getSolutions
  , incrLevel
  , level0
  )
where

import           DSpies.Prelude

import           Control.Monad.ST
import           Control.Monad.Yield.ST
import           Control.Monad.Yield.Class      ( MonadYield )
import qualified Data.Map                      as Map

import           BoolSat.Data

newtype CDCL s a =
    CDCL {
      unCDCL :: ExceptT Conflict (StateT (CDCLState s) (YieldST s Solution)) a
    }
  deriving (Functor, Applicative, Monad, MonadYield Solution)

newtype Level = Level Int
  deriving (Eq, Ord)
  deriving newtype Show

level0 :: Level
level0 = Level 0

incrLevel, decrLevel :: Level -> Level
incrLevel (Level n) = Level (n + 1)
decrLevel (Level n) = Level (n - 1)

data CDCLState s = CDCLState
  { assignments :: Map Variable AssignInfo
  , baseClauses :: [Disjunction]
  , learntClauses :: [Disjunction]
  , level :: Level
  , assignTimes :: NonEmpty [Variable]
  }

data AssignInfo = AssignInfo
  { value :: Sign
  , assignLevel :: Level
  , cause :: Maybe Disjunction
  }

data Conflict = Conflict {conflictRule :: Disjunction, conflictLevel :: Level}
  deriving (Show)

getSolutions :: Problem -> (forall s . CDCL s ()) -> [Solution]
getSolutions prob act0 = runYieldST (go act0)
 where
  go :: forall s . CDCL s () -> YieldST s Solution ()
  go act = do
    i <- liftST $ initialState prob
    _ <- (`runStateT` i) $ runExceptT $ unCDCL act
    return ()

initialState :: Problem -> ST s (CDCLState s)
initialState (Problem baseClauses) = return $ CDCLState
  { assignments   = Map.empty
  , baseClauses
  , learntClauses = []
  , level         = level0
  , assignTimes   = [] :| []
  }
