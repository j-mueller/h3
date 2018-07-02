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
import           Data.H3.Geo.Types     (Point (..), WGS84 (..))
import           Data.H3.Scalable      (Scalable (..))
import           Data.H3.Utils         (linear)

data Mercator a

-- | Null meridian used for mercator projection (in degrees)
newtype NullMeridian = NullMeridian { getNullMeridian :: Double }

-- | > greenwich = NullMeridian 0
greenwich :: NullMeridian
greenwich = NullMeridian 0

-- | @Iso@ 'NullMeridian' 'Double'
_NullMeridian :: forall p f. (Profunctor p, Functor f) => p NullMeridian (f NullMeridian) -> p Double (f Double)
_NullMeridian = dimap NullMeridian (fmap getNullMeridian)

-- | Construct a mercator projection that maps the given "rectangle" of
--   coordinates to the target rectangle
mercator :: (WGS84, WGS84) -> NullMeridian -> ScaleOptions Mercator WGS84 (Double, Double)
mercator = MercScale

instance Scalable Mercator WGS84 (Double, Double) where
  type Target Mercator = Identity
  type TargetRange Mercator (Double, Double) = (Extent Double, Extent Double)
  data ScaleOptions Mercator WGS84 (Double, Double) = MercScale (WGS84, WGS84) NullMeridian
  scale (MercScale ex (NullMeridian mrd)) tgt x = Identity (xScale lng', yScale lt') where
    mrd' = rad mrd
    ((minLong', minLat'), (maxLong', maxLat')) =
      bimap (proj . bimap rad rad . getPoint . getWGS84) (proj . bimap rad rad . getPoint . getWGS84) ex
    ((minX, maxX), (minY, maxY)) =
      bimap toTuple toTuple tgt
    (lng', lt') = proj $ bimap rad rad $ getPoint $ getWGS84 x
    xScale = linear (minLong', maxLong') (minX, maxX)
    yScale = linear (minLat', maxLat') (minY, maxY)

    -- mercator projection
    proj (long, lat) = (long - mrd', asinh $ tan lat)


-- | Convert a `Double` from degrees to radians.
rad :: Double -> Double
rad d = d * pi / 180
