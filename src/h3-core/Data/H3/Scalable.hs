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
-- Type classes for scales. A scale is a family of functions that are
-- parameterised by a target range (for example, the dimensions of the
-- diagram in pixels) and an additional set of options specific to each
-- scale.
--
-- In @h3@, 'Scalable' is the type class that represents scales.
-- Scales used for visualisation commonly have metadata such as legends, grid
-- lines etc. The 'Data.H3.Visuals.ChartVisuals' class deals with this kind of
-- data.
--------------------------------------------------------------------
module Data.H3.Scalable(
  -- * Scales
  Scalable(..),
  -- * Basic instances
  arrow
) where

import           Data.Functor.Identity (Identity (..))

-- | The class of scales. Each scale has an associated data type 'ScaleOptions'
--   that can be used to configure the scale.
class Scalable (f :: * -> *) a b where

  -- | 'Target' @f@ is the codomain of the scale. For example, if @f@ is a scale
  --   that maps real numbers to real numbers, then 'Target' @f@ is
  --   'Data.Functor.Identity' (a point). If it is an ordinal scale, then
  -- 'Target' @f@ is 'Data.H3.Extent.Extent' (an interval).
  type Target f :: * -> *

  -- | 'TargetRange' @f@ is the type of the range parameter of the scale. For
  --   example, if @f@ is a scale that maps real numbers to real numbers, then
  --   'TargetRange' @f@ is 'Data.H3.Extent' (an interval). For ordinal scales,
  --   'Target' @f@ is @[]@, a list of possible values.
  type TargetRange f b :: *

  -- | Additional parameters for the scale (other than target range).
  data ScaleOptions f a b :: *

  -- | Given 'ScaleOptions' and a 'TargetRange', produce a map 'a -> (Target f)
  --   b'.
  scale :: (ScaleOptions f) a b -> TargetRange f b -> a -> (Target f) b

-- | Every function 'f :: a -> b' is a scale
arrow :: (a -> b) -> ScaleOptions ((->) a) a b
arrow = ArrScaleOpts

instance Scalable ((->) a) a b where
  type Target ((->) a) = Identity
  type TargetRange ((->) a) b = ()
  data ScaleOptions ((->) a) a b = ArrScaleOpts (a -> b)
  scale (ArrScaleOpts f) _ = Identity . f

