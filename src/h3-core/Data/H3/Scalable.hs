{-# LANGUAGE DeriveFunctor         #-}
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
-- In visualisation commonly have metadata such as legends, grid
-- lines etc. The 'ChartVisuals' class deals with this kind of data.
--------------------------------------------------------------------
module Data.H3.Scalable(
  -- * Scales
  Scalable(..),
  Target,
  TargetRange,
  -- * Visual elements
  ChartVisuals(..),
  VisualElements(..)
) where

import           Data.H3.Shape  (Shape)
import           Data.Semigroup (Semigroup (..))

-- | 'Target' @f@ is the codomain of the scale. For example, if @f@ is a scale
--   that maps real numbers to real numbers, then 'Target' @f@ is
--   'Data.Functor.Identity' (a point). If it is an ordinal scale, then
-- 'Target' @f@ is 'Data.H3.Extent' (an interval).
type family Target (a :: * -> *) :: * -> * -- can be Identity or Extent

-- | 'TargetRange' @f@ is the type of the range parameter of the scale. For
--   example, if @f@ is a scale that maps real numbers to real numbers, then
--   'TargetRange' @f@ is 'Data.H3.Extent' (an interval). For ordinal scales,
--   'Target' @f@ is @[]@, a list of possible values.
type family TargetRange (a :: * -> *) :: * -> * -- Identity, Extent, ..

-- | The class of scales. Each scale has an associated data type 'ScaleOptions'
--   that can be used to configure the scale.
class Scalable f a b where

  -- | Additional parameters for the scale (other than target range).
  data ScaleOptions f :: * -> * -> *

  -- | Given 'ScaleOptions' and a 'TargetRange', produce a map 'a -> (Target f)
  --   b'.
  scale :: (ScaleOptions f) a b -> (TargetRange f) b -> a -> (Target f) b

-- | A collection of scale metadata that needs to be visualised. @n@ is the
-- type of coordinates in the target coordinate system, to allow combinators
-- such as 'Product' and 'Cartesian' to adjust the locations of the elements.
--
--  Note: The design of 'VisualElements' and 'ChartVisuals' is likely to
--  be changed in the future to make it more flexible.
data VisualElements s n = VisualElements {
  veTicks      :: [n], -- ^ Points in the target interval that should be indicated to the user
  veGridLines  :: [(n, n)], -- ^ Start and end points of grid lines
  veAxisLabels :: [(String, n)], -- ^ Points in the target interval that should be labelled
  veLegend     :: [Shape s n] -- ^ Additional data
} deriving (Functor)

instance Semigroup (VisualElements s n) where
  (VisualElements lt lg la ll) <> (VisualElements rt rg ra rl) =
    VisualElements (lt <> rt) (lg <> rg) (la <> ra) (ll <> rl)

instance Monoid (VisualElements s n) where
  mappend = (<>)
  mempty = VisualElements [] [] [] []

-- | The class of scales that have metadata in the form of 'VisualElements'.
class Scalable f a b => ChartVisuals f a b where
  visuals :: (ScaleOptions f) a b -> (TargetRange f) b ->  VisualElements String b
