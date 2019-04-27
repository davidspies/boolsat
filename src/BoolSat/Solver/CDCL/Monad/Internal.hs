module BoolSat.Solver.CDCL.Monad.Internal
  ( AssignInfo(..)
  , CDCL(..)
  , CDCLState(..)
  , Conflict(..)
  , DisjunctInfo(..)
  , Environment(..)
  , Level
  , VarInfo(..)
  , decrLevel
  , getSolutions
  , incrLevel
  , level0
  )
where

import           DSpies.Prelude

import           Control.Monad.Fix              ( mfix )
import           Control.Monad.ST
import           Control.Monad.ST.Class         ( MonadST )
import           Control.Monad.Yield.ST
import           Control.Monad.Yield.Class      ( MonadYield )
import qualified Data.DList                    as DList
import qualified Data.Map                      as Map
import           Data.STRef
import qualified Data.Set                      as Set

import           BoolSat.Data

newtype CDCL s a =
    CDCL {
      unCDCL ::
        ExceptT Conflict
        (StateT (CDCLState s)
        (ReaderT (Environment s)
        (YieldST s Solution))) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError Conflict
    , MonadReader (Environment s)
    , MonadST s
    , MonadState (CDCLState s)
    , MonadYield Solution
    )

newtype Level = Level Int
  deriving (Eq, Ord)
  deriving newtype Show

level0 :: Level
level0 = Level 0

incrLevel, decrLevel :: Level -> Level
incrLevel (Level n) = Level (n + 1)
decrLevel (Level n) = Level (n - 1)

data Environment s = Environment
  { assignments :: Map Variable (STRef s (VarInfo s))
  , baseClauses :: [STRef s (DisjunctInfo s)]
  }

data CDCLState s = CDCLState
  { learntClauses :: [STRef s (DisjunctInfo s)]
  , level :: Level
  , assignTimes :: NonEmpty [Variable]
  }

data VarInfo s = VarInfo
  { assigned :: Maybe AssignInfo
  , uses :: [STRef s (DisjunctInfo s)]
  }

newtype DisjunctInfo s = DisjunctInfo (Map Variable (Sign, STRef s (VarInfo s)))

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
    ie <- liftST $ initialEnv prob
    _  <- (`runReaderT` ie) $ (`runStateT` initialState) $ runExceptT $ unCDCL
      act
    return ()

initialEnv :: Problem -> ST s (Environment s)
initialEnv (Problem bc) = mfix $ \state -> do
  baseClauses <- forM bc $ \(Disjunction d) ->
    newSTRef $ DisjunctInfo $ Map.fromList $ map
      (\(Assignment k v) -> (k, (v, assignments state Map.! k)))
      (Set.toList d)
  let useMap = Map.unionsWith (<>) $ zipWith
        (\(Disjunction d) dr -> Map.fromList
          $ map (\(Assignment k _) -> (k, DList.singleton dr)) (Set.toList d)
        )
        bc
        baseClauses
  assignments <- mapM (newSTRef . VarInfo Nothing . DList.toList) useMap
  return $ Environment { assignments, baseClauses }

initialState :: CDCLState s
initialState =
  CDCLState { learntClauses = [], level = level0, assignTimes = [] :| [] }
