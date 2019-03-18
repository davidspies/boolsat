module BoolSat.Solver.CDCL.Data.LengthList
  ( LengthList
  , cons
  , length
  , toList
  , uncons
  )
where

import           DSpies.Prelude          hiding ( uncons )

import           Data.Semigroup

data LengthList a = LengthList
  { _toList :: [a]
  , _length :: Int
  }
  deriving (Functor)

instance Foldable LengthList where
  fold = fold . _toList
  foldMap fn = foldMap fn . _toList
  foldr fn base = foldr fn base . _toList
  foldr' fn base = foldr' fn base . _toList
  foldl fn base = foldl fn base . _toList
  foldl' fn base = foldl' fn base . _toList
  foldr1 fn = foldr1 fn . _toList
  foldl1 fn = foldl1 fn . _toList
  toList = _toList
  null   = null . _toList
  length = _length
  elem x = elem x . _toList
  maximum = maximum . _toList
  minimum = minimum . _toList
  sum     = sum . _toList
  product = product . _toList
instance Traversable LengthList where
  traverse fn LengthList {..} =
    LengthList <$> traverse fn _toList <*> pure _length
  sequenceA LengthList {..} = LengthList <$> sequenceA _toList <*> pure _length
  mapM fn LengthList {..} = LengthList <$> mapM fn _toList <*> pure _length
  sequence LengthList {..} = LengthList <$> sequence _toList <*> pure _length

instance Semigroup (LengthList a) where
  (<>) (LengthList x m) (LengthList y n) = LengthList (x <> y) (m + n)
  stimes n (LengthList x m) = LengthList (stimes n x) (fromIntegral n * m)
instance Monoid (LengthList a) where
  mempty = LengthList mempty 0
  mappend (LengthList x m) (LengthList y n) =
    LengthList (x `mappend` y) (m + n)

instance Applicative LengthList where
  pure x = LengthList (pure x) 1
  (<*>) (LengthList fs m) (LengthList xs n) = LengthList (fs <*> xs) (m * n)
instance Monad LengthList where
  (>>=) (LengthList xs _) fn = mconcat $ map fn xs

instance Alternative LengthList where
  empty = LengthList empty 0
  (<|>) (LengthList xs m) (LengthList ys n) = LengthList (xs <|> ys) (m + n)

cons :: (a -> LengthList a -> LengthList a)
cons x (LengthList xs n) = LengthList (x : xs) (n + 1)

uncons :: LengthList a -> Maybe (a, LengthList a)
uncons (LengthList xs n) = case xs of
  []        -> Nothing
  (x : xs') -> Just (x, LengthList xs' (n - 1))
