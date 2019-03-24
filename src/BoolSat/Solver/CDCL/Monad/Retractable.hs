module BoolSat.Solver.CDCL.Monad.Retractable
  ( RetractRef
  , RetractTracker
  , newRetractRef
  , newTracker
  , readRetractRef
  , upLevel
  , writeRetractRef
  )
where

import           DSpies.Prelude

import           Control.Monad.ST.Class
import           Control.Monad.ST
import           Data.STRef

import           BoolSat.Solver.CDCL.Data.LengthList
                                                ( LengthList )
import qualified BoolSat.Solver.CDCL.Data.LengthList
                                               as LengthList
import           BoolSat.Solver.CDCL.Monad.Mask

newtype RetractTracker s = RetractTracker (STRef s (LengthList [SomeRef s]))

data RetractRef s a = RetractRef (RetractTracker s) (STRef s [(Int, a)])

data SomeRef s = forall a . SomeRef (RetractRef s a)

newTracker :: ST s (RetractTracker s)
newTracker = RetractTracker <$> newSTRef empty

newRetractRef :: RetractTracker s -> a -> ST s (RetractRef s a)
newRetractRef tracker x = RetractRef tracker <$> newSTRef [(0, x)]

readRetractRef :: RetractRef s a -> ST s a
readRetractRef (RetractRef _ ref) = snd . head <$> readSTRef ref

upLevel
  :: (MonadMaskBase m, MonadST m, MonadST (Base m), World m ~ World (Base m))
  => RetractTracker (World m)
  -> m a
  -> m a
upLevel (RetractTracker ref) act = do
  liftST $ modifySTRef ref (LengthList.cons [])
  act `finally` liftST
    (do
      (h, t) <- fromJust . LengthList.uncons <$> readSTRef ref
      forM_ h $ \(SomeRef (RetractRef _ xref)) -> modifySTRef xref tail
      writeSTRef ref t
    )

writeRetractRef :: RetractRef s a -> a -> ST s ()
writeRetractRef xrr@(RetractRef (RetractTracker trackerRef) xref) x = do
  curlen <- length <$> readSTRef trackerRef
  xl     <- readSTRef xref
  let ((i, _), xt) = fromJust $ uncons xl
  if i < curlen
    then do
      modifySTRef trackerRef $ LengthList.uncons >>> fromJust >>> \(h, t) ->
        LengthList.cons (SomeRef xrr : h) t
      writeSTRef xref $ (i, x) : xl
    else writeSTRef xref $ (i, x) : xt
