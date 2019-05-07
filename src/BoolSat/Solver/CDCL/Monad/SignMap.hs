module BoolSat.Solver.CDCL.Monad.SignMap
    ( SignMap
    , map
    , modify
    , signMap
    , (!)
    )
where

import           DSpies.Prelude          hiding ( map )

import           BoolSat.Data

data SignMap a = SignMap {negItem :: a, posItem :: a}
    deriving (Functor)

signMap :: a -> a -> SignMap a
signMap negItem posItem = SignMap { negItem, posItem }

(!) :: SignMap a -> Sign -> a
(!) SignMap { negItem } (Sign False) = negItem
(!) SignMap { posItem } (Sign True ) = posItem

modify :: Sign -> (a -> a) -> SignMap a -> SignMap a
modify (Sign False) vop sm@SignMap { negItem } = sm { negItem = vop negItem }
modify (Sign True ) vop sm@SignMap { posItem } = sm { posItem = vop posItem }

map :: (a -> b) -> SignMap a -> SignMap b
map = fmap

instance Semigroup a => Semigroup (SignMap a) where
    (<>) (SignMap xl yl) (SignMap xr yr) = SignMap (xl <> xr) (yl <> yr)
