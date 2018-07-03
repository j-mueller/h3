{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.H3.Geo.Types(
  Point(..),
  _Point,
  Polygon(..),
  _Polygon,
  WGS84(..),
  _WGS84,
  Radians(..),
  _Radians,
  Degrees(..),
  _Degrees,
  _DegRad,
  fromRad,
  toRad
  ) where

import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Profunctor         (Profunctor (..))
import           Data.Semigroup.Foldable (Foldable1 (..))
import           GHC.Generics            (Generic)

newtype Point a = Point { getPoint :: (a, a) }
  deriving (Eq, Ord, Show, Generic, Functor)

-- | Point iso
_Point :: forall p f a b. (Profunctor p, Functor f) => p (Point a) (f (Point b)) -> p (a, a) (f (b, b))
_Point = dimap Point (fmap getPoint)

newtype Polygon a = Polygon { getPolygon :: NonEmpty a }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Foldable1)

-- | Polygon iso
_Polygon :: forall p f a b. (Profunctor p, Functor f) => p (Polygon a) (f (Polygon b)) -> p (NonEmpty a) (f (NonEmpty b))
_Polygon = dimap Polygon (fmap getPolygon)

newtype WGS84 = WGS84 { getWGS84 :: Point Degrees }
  deriving (Eq, Ord, Show, Generic)

-- | Point in WGS84 format
_WGS84 :: forall p f. (Profunctor p, Functor f) => p WGS84 (f WGS84) -> p (Point Degrees) (f (Point Degrees))
_WGS84 = dimap WGS84 (fmap getWGS84)

newtype Radians = Radians { getRadians :: Double }
  deriving (Eq, Ord, Show, Generic, Num, Fractional, Floating)

-- | Angle in radians
_Radians :: forall p f. (Profunctor p, Functor f) => p Radians (f Radians) -> p Double (f Double)
_Radians = dimap Radians (fmap getRadians)

newtype Degrees = Degrees { getDegrees :: Double }
  deriving (Eq, Ord, Show, Generic, Num, Fractional, Floating)

-- | Angle in degrees
_Degrees :: forall p f. (Profunctor p, Functor f) => p Degrees (f Degrees) -> p Double (f Double)
_Degrees = dimap Degrees (fmap getDegrees)

toRad :: Degrees -> Radians
toRad (Degrees d) = Radians $ d * pi / 180

fromRad :: Radians -> Degrees
fromRad (Radians r) = Degrees $ r * 180 / pi

_DegRad :: forall p f. (Profunctor p, Functor f) => p Degrees (f Degrees) -> p Radians (f Radians)
_DegRad = dimap fromRad (fmap toRad)
