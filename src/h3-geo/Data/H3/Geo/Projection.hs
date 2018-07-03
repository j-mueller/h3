{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Scales for projecting geographic coordinates onto a 2D plane.
--------------------------------------------------------------------
module Data.H3.Geo.Projection(
  -- * Projections
  Mercator,
  mercator,
  NullMeridian(..),
  greenwich,
  -- * Data constructors
  ScaleOptions(..)
  ) where

import           Data.Bifunctor        (Bifunctor (..))
import           Data.Functor.Identity (Identity (..))
import           Data.Profunctor       (Profunctor (..))

import           Data.H3.Extent        (Extent (..), toTuple)
import           Data.H3.Geo.Types     (Degrees (..), Point (..), Radians (..),
                                        toRad)
import           Data.H3.Scalable      (Scalable (..))
import           Data.H3.Utils         (linear)

data Mercator a

-- | Null meridian used for mercator projection (in degrees)
newtype NullMeridian = NullMeridian { getNullMeridian :: Degrees }

-- | > greenwich = NullMeridian 0
greenwich :: NullMeridian
greenwich = NullMeridian 0

-- | @Iso@ 'NullMeridian' 'Degrees'
_NullMeridian :: forall p f. (Profunctor p, Functor f) => p NullMeridian (f NullMeridian) -> p Degrees (f Degrees)
_NullMeridian = dimap NullMeridian (fmap getNullMeridian)

-- | Construct a mercator projection that maps the given "rectangle" of
--   coordinates to the target rectangle
mercator :: (Point Radians, Point Radians) -> NullMeridian -> ScaleOptions Mercator (Point Radians) (Double, Double)
mercator = MercScale

instance Scalable Mercator (Point Radians) (Double, Double) where
  type Target Mercator = Identity
  type TargetRange Mercator (Double, Double) = (Extent Double, Extent Double)
  data ScaleOptions Mercator (Point Radians) (Double, Double) =
    MercScale
      (Point Radians, Point Radians)
      NullMeridian
  scale (MercScale ex (NullMeridian mrd)) tgt x = Identity (xScale lng', yScale lt') where
    mrd' = toRad mrd
    ((minLong', minLat'), (maxLong', maxLat')) =
      bimap proj proj ex
    ((minX, maxX), (minY, maxY)) =
      bimap toTuple toTuple tgt
    (lng', lt') = proj x
    xScale = linear (minLong', maxLong') (minX, maxX)
    yScale = linear (minLat', maxLat') (minY, maxY)

    -- mercator projection
    proj :: Point Radians -> (Double, Double)
    proj (Point (long, lat)) = bimap getRadians getRadians (long - mrd', asinh $ tan lat)
