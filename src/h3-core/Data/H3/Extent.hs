{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.H3.Extent(
  Extent(..),
  _Extent,
  extent,
  toTuple,
  fromTuple
  ) where

import           Data.Profunctor         (Profunctor (..))
import           Data.Semigroup          (Max (..), Min (..), Semigroup (..))
import           Data.Semigroup.Foldable (Foldable1 (..))
import           GHC.Generics            (Generic)

newtype Extent a = Extent { getExtent :: (Min a, Max a) }
  deriving (Eq, Ord, Show, Functor, Semigroup, Generic, Foldable, Traversable)

instance Foldable1 Extent where
  fold1 (Extent (Min l, Max r)) = l <> r

_Extent :: forall p f a b. (Profunctor p, Functor f) => p (Extent a) (f (Extent b)) -> p (a, a) (f (b, b))
_Extent = dimap fromTuple (fmap toTuple)

extent :: a -> Extent a
extent a = Extent (Min a, Max a)

toTuple :: Extent a -> (a, a)
toTuple (Extent (Min l, Max r)) = (l, r)

fromTuple :: (a, a) -> Extent a
fromTuple (l, r) = Extent (Min l, Max r)
