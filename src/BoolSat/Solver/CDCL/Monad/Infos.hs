module BoolSat.Solver.CDCL.Monad.Infos
    ( AssignInfo(..)
    , ProblemState
    , addDisjunction
    , assignVar
    , getAndClearPropagators
    , getAndClearPures
    , getBaseDisjunctions
    , getLearntDisjunctions
    , getVariables
    , initialize
    , unassignVar
    )
where

import           DSpies.Prelude

import           Control.Monad.Fix              ( mfix )
import           Control.Monad.ST
import qualified Data.DList                    as DList
import qualified Data.Map                      as Map
import           Data.STRef
import qualified Data.Set                      as Set
import           Data.Traversable               ( for )

import           BoolSat.Data
import           BoolSat.Solver.CDCL.Monad.SignMap
                                                ( SignMap
                                                , signMap
                                                )
import qualified BoolSat.Solver.CDCL.Monad.SignMap
                                               as SignMap

newtype Level = Level Int
    deriving (Eq, Ord)
    deriving newtype Show

data AssignInfo = AssignInfo
    { value :: Sign
    , assignLevel :: Level
    , cause :: Maybe Disjunction
    }

type VarStats = SignMap Int
data VarInfo s = VarInfo
    { assignInfo :: Maybe AssignInfo
    , containingDisjunctions :: SignMap [DisjunctionRef s]
    , varStats :: VarStats
    }
type VarInfoRef s = STRef s (VarInfo s)

type DisjunctionMap s = Map Variable (Sign, VarInfoRef s)

data DisjunctionStats = DisjunctionStats{remaining :: Int, satisfying :: Int}
instance Semigroup DisjunctionStats where
    (<>) (DisjunctionStats lr ls) (DisjunctionStats rr rs) =
        DisjunctionStats (lr + rr) (ls + rs)
instance Monoid DisjunctionStats where
    mempty = DisjunctionStats 0 0

data DisjunctionInfo s = DisjunctionInfo
    { disjunctionMap :: DisjunctionMap s
    , disjunctionStats :: DisjunctionStats
    }
type DisjunctionRef s = STRef s (DisjunctionInfo s)

data ProblemState' s = ProblemState'
    { varMap :: Map Variable (VarInfoRef s)
    , baseDisjunctions :: [DisjunctionRef s]
    , learntDisjunctions :: [DisjunctionRef s]
    , pendingPropagators :: [Disjunction]
    , pendingPureVars :: [Variable]
    }
type ProblemStateRef s = STRef s (ProblemState' s)
newtype ProblemState s = ProblemState (ProblemStateRef s)

initialize :: Problem -> ST s (ProblemState s)
initialize prob@(Problem disjunctions) = do
    ps <- mfix $ \ps -> do
        let variables = Set.toList $ allVars prob
        baseDisjunctions <-
            forM disjunctions $ \(Disjunction (Set.toList -> xs)) -> do
                let disjunctionMap = Map.fromList $ map
                        (\(Assignment k v) -> (k, (v, varMap ps Map.! k)))
                        xs
                    disjunctionStats = DisjunctionStats { remaining  = length xs
                                                        , satisfying = 0
                                                        }
                newSTRef DisjunctionInfo { .. }
        let
            makeMap r = do
                DisjunctionInfo { disjunctionMap } <- readSTRef r
                return
                    $ Map.fromList
                    $ map
                          (\(x, (sig, _)) ->
                              (x, singleSignMap sig (DList.singleton r))
                          )
                    $ Map.toList disjunctionMap
        varMapProto <-
            fmap (fmap DList.toList)
            .   Map.unionsWith (<>)
            <$> mapM makeMap baseDisjunctions
        varMap <- for varMapProto $ \containingDisjunctions -> do
            let varStats = SignMap.map length containingDisjunctions
            newSTRef VarInfo { assignInfo             = Nothing
                             , containingDisjunctions
                             , varStats
                             }
        let learntDisjunctions = []
            pendingPropagators = disjunctions
            pendingPureVars    = variables
        return ProblemState' { .. }
    ProblemState <$> newSTRef ps

data Connection = Unset | Matching | Against
    deriving (Eq)

singleSignMap :: Monoid a => Sign -> a -> SignMap a
singleSignMap = \case
    Sign False -> signMap mempty
    Sign True  -> (`signMap` mempty)

addDisjunction :: ProblemState s -> Disjunction -> ST s ()
addDisjunction (ProblemState psref) (Disjunction (Set.toList -> xs)) = do
    ps@ProblemState' { learntDisjunctions, varMap } <- readSTRef psref
    let dl = map (\(Assignment k v) -> (k, (v, varMap Map.! k))) xs
    connections <- forM dl $ \(_, (v, vir)) -> do
        VarInfo { assignInfo } <- readSTRef vir
        return $ case assignInfo of
            Nothing           -> Unset
            Just AssignInfo { value } | v == value -> Matching
            Just AssignInfo{} -> Against
    let disjunctionMap   = Map.fromList dl
        satisfying       = length $ filter (== Matching) connections
        disjunctionStats = DisjunctionStats
            { remaining  = length $ filter (== Unset) connections
            , satisfying
            }
        di = DisjunctionInfo { disjunctionMap, disjunctionStats }
    dir <- newSTRef di
    writeSTRef psref ps { learntDisjunctions = dir : learntDisjunctions }
    forM_ dl $ \(_, (v, vir)) -> do
        vi@VarInfo { containingDisjunctions, varStats } <- readSTRef vir
        let newDisjunctions = SignMap.modify v (dir :) containingDisjunctions
            newVarStats     = if satisfying > 0
                then varStats
                else SignMap.modify v (+ 1) varStats
        writeSTRef
            vir
            vi { containingDisjunctions = newDisjunctions
               , varStats               = newVarStats
               }

assignVar :: HasCallStack => ProblemState s -> Variable -> AssignInfo -> ST s ()
assignVar (ProblemState psref) k v@AssignInfo { value } = do
    ProblemState' { varMap } <- readSTRef psref
    let vref = varMap Map.! k
    vi@VarInfo { assignInfo, containingDisjunctions } <- readSTRef vref
    case assignInfo of
        Just _  -> error $ show k ++ " already assigned"
        Nothing -> do
            mapM_ (incrSatisfied psref) (containingDisjunctions SignMap.! value)
            mapM_ (decrRemaining psref)
                  (containingDisjunctions SignMap.! opp value)
            writeSTRef vref vi { assignInfo = Just v }

incrSatisfied, decrSatisfied, incrRemaining, decrRemaining
    :: ProblemStateRef s -> DisjunctionRef s -> ST s ()
incrSatisfied = _
decrSatisfied = _
incrRemaining = _
decrRemaining = _

unassignVar :: HasCallStack => ProblemState s -> Variable -> ST s AssignInfo
unassignVar (ProblemState psref) k = do
    ProblemState' { varMap } <- readSTRef psref
    let vref = varMap Map.! k
    vi@VarInfo { assignInfo, containingDisjunctions } <- readSTRef vref
    case assignInfo of
        Nothing                     -> error $ show k ++ " not assigned"
        Just v@AssignInfo { value } -> do
            mapM_ (decrSatisfied psref) (containingDisjunctions SignMap.! value)
            mapM_ (incrRemaining psref)
                  (containingDisjunctions SignMap.! opp value)
            writeSTRef vref vi { assignInfo = Nothing }
            return v

getAndClearPropagators :: ProblemState s -> ST s [Disjunction]
getAndClearPropagators (ProblemState psref) = do
    state@ProblemState' { pendingPropagators } <- readSTRef psref
    writeSTRef psref state { pendingPropagators = [] }
    return pendingPropagators

getAndClearPures :: ProblemState s -> ST s [Variable]
getAndClearPures (ProblemState psref) = do
    state@ProblemState' { pendingPureVars } <- readSTRef psref
    writeSTRef psref state { pendingPureVars = [] }
    return pendingPureVars

getVariables :: ProblemState s -> ST s [Variable]
getVariables (ProblemState psref) =
    readSTRef psref <&> \ProblemState' { varMap } -> Map.keys varMap

reproduceDisjunction :: DisjunctionRef s -> ST s Disjunction
reproduceDisjunction d =
    readSTRef d <&> \DisjunctionInfo { disjunctionMap } ->
        Disjunction
            $ Set.fromList
            $ map (\(k, (v, _)) -> Assignment k v)
            $ Map.toList disjunctionMap

getBaseDisjunctions :: ProblemState s -> ST s [Disjunction]
getBaseDisjunctions (ProblemState psref) = do
    ProblemState' { baseDisjunctions } <- readSTRef psref
    mapM reproduceDisjunction baseDisjunctions

getLearntDisjunctions :: ProblemState s -> ST s [Disjunction]
getLearntDisjunctions (ProblemState psref) = do
    ProblemState' { learntDisjunctions } <- readSTRef psref
    mapM reproduceDisjunction learntDisjunctions
