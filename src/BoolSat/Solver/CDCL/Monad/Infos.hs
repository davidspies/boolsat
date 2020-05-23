{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

import           DSpies.Prelude          hiding ( (<&>) )

import           Control.Lens            hiding ( Level )
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
    { _value :: Sign
    , _assignLevel :: Level
    , _cause :: Maybe Disjunction
    }
makeLenses ''AssignInfo

type VarStats = SignMap Int
data VarInfo s = VarInfo
    { _assignInfo :: Maybe AssignInfo
    , _containingDisjunctions :: SignMap [DisjunctionRef s]
    , _varStats :: VarStats
    }
type VarInfoRef s = STRef s (VarInfo s)

type DisjunctionMap s = Map Variable (Sign, VarInfoRef s)

data DisjunctionStats = DisjunctionStats{_remaining :: Int, _satisfying :: Int}

instance Semigroup DisjunctionStats where
    (<>) (DisjunctionStats lr ls) (DisjunctionStats rr rs) =
        DisjunctionStats (lr + rr) (ls + rs)
instance Monoid DisjunctionStats where
    mempty = DisjunctionStats 0 0

data DisjunctionInfo s = DisjunctionInfo
    { _disjunctionMap :: DisjunctionMap s
    , _disjunctionStats :: DisjunctionStats
    }
type DisjunctionRef s = STRef s (DisjunctionInfo s)

makeLenses ''DisjunctionInfo
makeLenses ''DisjunctionStats
makeLenses ''VarInfo

data ProblemState' s = ProblemState'
    { _varMap :: Map Variable (VarInfoRef s)
    , _baseDisjunctions :: [DisjunctionRef s]
    , _learntDisjunctions :: [DisjunctionRef s]
    , _pendingPropagators :: [Disjunction]
    , _pendingPureVars :: Set Variable
    }
makeLenses ''ProblemState'
type ProblemStateRef s = STRef s (ProblemState' s)
newtype ProblemState s = ProblemState (ProblemStateRef s)

initialize :: Problem -> ST s (ProblemState s)
initialize prob@(Problem disjunctions) = do
    state <- mfix $ \state -> do
        let variables = Set.toList $ allVars prob
        _baseDisjunctions <-
            forM disjunctions $ \(Disjunction (Set.toList -> xs)) -> do
                let mkInfo (Assignment k v) =
                        (k, (v, (state ^. varMap) Map.! k))
                    _disjunctionMap   = Map.fromList $ map mkInfo xs
                    _disjunctionStats = DisjunctionStats
                        { _remaining  = length xs
                        , _satisfying = 0
                        }
                newSTRef DisjunctionInfo { .. }
        let
            makeMap r = do
                dinfo <- readSTRef r
                return
                    $ Map.fromList
                    $ map
                          (\(x, (sig, _)) ->
                              (x, singleSignMap sig (DList.singleton r))
                          )
                    $ Map.toList (dinfo ^. disjunctionMap)
        varMapProto <-
            fmap (fmap DList.toList)
            .   Map.unionsWith (<>)
            <$> mapM makeMap _baseDisjunctions
        _varMap <- for varMapProto $ \cds -> do
            let vs = SignMap.map length cds
            newSTRef VarInfo { _assignInfo             = Nothing
                             , _containingDisjunctions = cds
                             , _varStats               = vs
                             }
        let _learntDisjunctions = []
            _pendingPropagators = disjunctions
            _pendingPureVars    = Set.fromList variables
        return ProblemState' { .. }
    ProblemState <$> newSTRef state

data Connection = Unset | Matching | Against
    deriving (Eq)

singleSignMap :: Monoid a => Sign -> a -> SignMap a
singleSignMap = \case
    Sign False -> signMap mempty
    Sign True  -> (`signMap` mempty)

addDisjunction :: ProblemState s -> Disjunction -> ST s ()
addDisjunction (ProblemState psref) (Disjunction (Set.toList -> xs)) = do
    ps <- readSTRef psref
    let dl = map (\(Assignment k v) -> (k, (v, (ps ^. varMap) Map.! k))) xs
    connections <- forM dl $ \(_, (v, vir)) -> do
        vi <- readSTRef vir
        return $ case vi ^. assignInfo of
            Nothing                      -> Unset
            Just inf | v == inf ^. value -> Matching
            Just AssignInfo{}            -> Against
    let _disjunctionMap   = Map.fromList dl
        _satisfying       = length $ filter (== Matching) connections
        _disjunctionStats = DisjunctionStats
            { _remaining  = length $ filter (== Unset) connections
            , _satisfying
            }
        di = DisjunctionInfo { _disjunctionMap, _disjunctionStats }
    dir <- newSTRef di
    writeSTRef psref $ ps & learntDisjunctions %~ (dir :)
    forM_ dl $ \(_, (v, vir)) -> do
        vi <- readSTRef vir
        writeSTRef vir
            $ vi
            & (containingDisjunctions %~ SignMap.modify v (dir :))
            & (  varStats
              %~ if _satisfying > 0 then id else SignMap.modify v (+ 1)
              )

assignVar :: HasCallStack => ProblemState s -> Variable -> AssignInfo -> ST s ()
assignVar (ProblemState psref) k v@AssignInfo { _value } = do
    ps <- readSTRef psref
    let vref = (ps ^. varMap) Map.! k
    vi <- readSTRef vref
    case vi ^. assignInfo of
        Just _  -> error $ show k ++ " already assigned"
        Nothing -> do
            mapM_ (incrSatisfied psref)
                  ((vi ^. containingDisjunctions) SignMap.! _value)
            mapM_ (decrRemaining psref)
                  ((vi ^. containingDisjunctions) SignMap.! opp _value)
            writeSTRef vref $ vi & assignInfo ?~ v

incrSatisfied, decrSatisfied, incrRemaining, decrRemaining
    :: forall s . ProblemStateRef s -> DisjunctionRef s -> ST s ()
incrSatisfied psref dref = do
    dinfo <- readSTRef dref
    writeSTRef dref (dinfo & disjunctionStats . satisfying +~ 1)
    when (dinfo ^. disjunctionStats . satisfying == 0)
        $ forM_ (Map.toList (dinfo ^. disjunctionMap))
        $ \(k, (sig, ref)) -> do
              vInfo <- readSTRef ref
              let newval = (vInfo ^. varStats) SignMap.! sig - 1
              writeSTRef ref $ vInfo & varStats %~ SignMap.overwrite sig newval
              when (newval == 0)
                  $  modifySTRef psref
                  $  pendingPureVars
                  %~ (Set.insert k)
decrSatisfied = _
incrRemaining = _
decrRemaining = _

unassignVar :: HasCallStack => ProblemState s -> Variable -> ST s AssignInfo
unassignVar (ProblemState psref) k = do
    ps <- readSTRef psref
    let vref = (ps ^. varMap) Map.! k
    vi <- readSTRef vref
    case vi ^. assignInfo of
        Nothing -> error $ show k ++ " not assigned"
        Just v  -> do
            let val = v ^. value
            mapM_ (decrSatisfied psref)
                  ((vi ^. containingDisjunctions) SignMap.! val)
            mapM_ (incrRemaining psref)
                  ((vi ^. containingDisjunctions) SignMap.! opp val)
            writeSTRef vref $ vi & assignInfo .~ Nothing
            return v

getAndClearPropagators :: ProblemState s -> ST s [Disjunction]
getAndClearPropagators (ProblemState psref) = do
    state <- readSTRef psref
    writeSTRef psref $ state & pendingPropagators .~ []
    return $ state ^. pendingPropagators

getAndClearPures :: ProblemState s -> ST s (Set Variable)
getAndClearPures (ProblemState psref) = do
    state <- readSTRef psref
    writeSTRef psref $ state & pendingPureVars .~ Set.empty
    return (state ^. pendingPureVars)

getVariables :: ProblemState s -> ST s [Variable]
getVariables (ProblemState psref) =
    readSTRef psref <&> \ps -> Map.keys $ ps ^. varMap

reproduceDisjunction :: DisjunctionRef s -> ST s Disjunction
reproduceDisjunction d = readSTRef d <&> \dinfo ->
    Disjunction
        $ Set.fromList
        $ map (\(k, (v, _)) -> Assignment k v)
        $ Map.toList (dinfo ^. disjunctionMap)

getBaseDisjunctions :: ProblemState s -> ST s [Disjunction]
getBaseDisjunctions (ProblemState psref) = do
    state <- readSTRef psref
    mapM reproduceDisjunction (state ^. baseDisjunctions)

getLearntDisjunctions :: ProblemState s -> ST s [Disjunction]
getLearntDisjunctions (ProblemState psref) = do
    state <- readSTRef psref
    mapM reproduceDisjunction (state ^. learntDisjunctions)
