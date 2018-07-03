{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  Albers,
  albers,
  albers',
  -- * Combinators
  MapScale,
  mapScale,
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

-- | Construct a mercator projection from a null meridian
mercator :: NullMeridian -> ScaleOptions Mercator (Point Radians) (Double, Double)
mercator = MercScale

instance Scalable Mercator (Point Radians) (Double, Double) where
  type Target Mercator = Identity
  type TargetRange Mercator (Double, Double) = ()
  data ScaleOptions Mercator (Point Radians) (Double, Double) =
    MercScale NullMeridian
  scale (MercScale (NullMeridian mrd)) _ = Identity . proj where
    mrd' = toRad mrd

    -- mercator projection
    proj :: Point Radians -> (Double, Double)
    proj (Point (long, lat)) = bimap getRadians getRadians (long - mrd', asinh $ tan lat)

-- | Albers projection (https://en.wikipedia.org/wiki/Albers_projection).
-- Sensible values for first and second standard parallels are
-- 20°N/50°N or 15°N/45°N
albers ::
     Point Radians -- ^ Reference point
  -> Radians -- ^ First standard parallel
  -> Radians -- ^ Second standard parallel
  -> ScaleOptions Albers (Point Radians) (Double, Double)
albers = AlbScale

-- | Albers projection (https://en.wikipedia.org/wiki/Albers_projection) using
--   15 and 45 degrees for the standard parallels.
albers' ::
    Point Radians -- ^ Reference point
  -> ScaleOptions Albers (Point Radians) (Double, Double)
albers' b = AlbScale b (toRad 15) (toRad 45)

data Albers a

instance Scalable Albers (Point Radians) (Double, Double) where
  type Target Albers = Identity
  type TargetRange Albers (Double, Double) = ()
  data ScaleOptions Albers (Point Radians) (Double, Double) =
    AlbScale {
      alReferencePoint :: Point Radians, -- reference point
      alPhi1 :: Radians, -- First standard parallel
      alPhi2 :: Radians -- Second standard parallel
    }
  scale (AlbScale ref phi1 phi2) _ = Identity . proj where
    (refLong, refLat) = getPoint ref

    -- albers projection
    proj :: Point Radians -> (Double, Double)
    proj (Point (long, lat)) = bimap getRadians getRadians (rho * sin theta, rho0 - rho * cos theta) where
      n = (sin phi1 + sin phi2) / 2
      theta = n * (long - refLong)
      c = cos phi1 ^ (2 :: Integer) + 2 * n * sin phi1
      rho = sqrt (c - 2 * n * sin lat) / n
      rho0 = sqrt (c - 2 * n * sin refLat) / n

data MapScale (f :: * -> *) a

-- | Scale the points of a map so that the "rectangle" specified by the two
--   points maps to the target range.
--
--   NOTE: This does not preserve the aspect ratio (you need to ensure the
--         target dimensions are correct)
--
mapScale ::
  (Point Radians, Point Radians) -- ^ SW and NE corners of the source rectangle
  -> (ScaleOptions f) (Point Radians) (Double, Double) -- ^ Inner scale
  -> ScaleOptions (MapScale f) (Point Radians) (Double, Double)
mapScale = MapScale

instance (
  Target f ~ Identity,
  TargetRange f (Double, Double) ~ (),
  Scalable f (Point Radians) (Double, Double)
  ) => Scalable (MapScale f) (Point Radians) (Double, Double) where
    type Target (MapScale f) = Identity
    type TargetRange (MapScale f) (Double, Double) = (Extent Double, Extent Double)
    data ScaleOptions (MapScale f) (Point Radians) (Double, Double) =
      MapScale (Point Radians, Point Radians) ((ScaleOptions f) (Point Radians) (Double, Double))
    scale (MapScale src opts) tgt = fmap (bimap xScale yScale) . scale' where
      scale' = scale opts ()
      (Identity (minLong', minLat'), Identity (maxLong', maxLat')) =
        bimap scale' scale' src
      ((minX, maxX), (minY, maxY)) =
        bimap toTuple toTuple tgt
      xScale = linear (minLong', maxLong') (minX, maxX)
      yScale = linear (minLat', maxLat') (minY, maxY)
