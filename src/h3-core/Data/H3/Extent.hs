{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Data.H3.Extent(
  Extent(..),
  _Extent,
  extent,
  toTuple,
  fromTuple,
  resize
  ) where

import           Data.IntervalMap.Generic.Interval (Interval (..))
import           Data.Profunctor                   (Profunctor (..))
import           Data.Semigroup                    (Max (..), Min (..),
                                                    Semigroup (..))
import           Data.Semigroup.Foldable           (Foldable1 (..))
import           GHC.Generics                      (Generic)

newtype Extent a = Extent { getExtent :: (Min a, Max a) }
  deriving (Eq, Ord, Show, Functor, Semigroup, Generic, Foldable, Traversable)

instance Foldable1 Extent where
  fold1 (Extent (Min l, Max r)) = l <> r

instance Ord a => Interval (Extent a) a where
  lowerBound = fst . toTuple
  upperBound = snd . toTuple

_Extent :: forall p f a b. (Profunctor p, Functor f) => p (Extent a) (f (Extent b)) -> p (a, a) (f (b, b))
_Extent = dimap fromTuple (fmap toTuple)

extent :: a -> Extent a
extent a = Extent (Min a, Max a)

toTuple :: Extent a -> (a, a)
toTuple (Extent (Min l, Max r)) = (l, r)

fromTuple :: (a, a) -> Extent a
fromTuple (l, r) = Extent (Min l, Max r)

-- | Change the size of an [[Extent]] symmetrically
resize :: (Fractional a, Num a) => a -> Extent a -> Extent a
resize factor (Extent (Min mn, Max mx)) = Extent (Min mn', Max mx') where
  range = mx - mn
  change = ((range * factor) - range) / 2
  mn' = mn - change
  mx' = mx + change
