{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- A set of useful scales for visualising data. Each scale is identified by an
-- empty data type ('Continuous', 'Ordinal' etc). To use a scale @f@ to map
-- @a@s to @b@s you need to construct a value of @ScaleOptions f a b@. To
-- make this easier, a constructor such as 'continuous', 'ordinal', etc.  is
-- provided for each scale.
--
--
-- Scales can be combined in a number of ways, just like to ordinary functions.
-- For example, we can take the 'Product' of any two scales @f@ and @g@.
-- The 'ScaleOptions' of combined scales are usually functions of the
-- 'ScaleOptions' of their components, and can be constructed in the same way.
--------------------------------------------------------------------
module Data.H3.Scales(
  -- * Basic scales
  arrow,
  continuous,
  Continuous,
  IncludeZeroPolicy(..),
  cardinal,
  Cardinal,
  ordinal,
  Ordinal,
  -- * Combinators
  product,
  Product(..),
  nested,
  Nested,
  transformed,
  Transformed,
  anchored,
  Anchored,
  split,
  Split,
  Pair(..),
  ProductV,
  -- * Data family instance constructors
  ScaleOptions(..)
) where

import           Data.Functor.Compose    (Compose (..))
import           Data.Functor.Identity   (Identity (..))
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Map                as Map
import           Data.Profunctor         (Profunctor (..))
import           Data.Semigroup          ((<>))
import           Data.Semigroup.Foldable (Foldable1 (..))
import           Data.Void               (Void)

import           Data.H3.Extent          (Extent, extent, fromTuple, toTuple)
import           Data.H3.Scalable        (Scalable (..), ScaleOptions, Target,
                                          TargetRange, arrow)
import           Data.H3.Utils           (defaultLabelCount, linear,
                                          looseLabels)
import           Prelude                 hiding (product)


-- | Indicates whether to extend the source (domain) of a continuous scale to
-- include 0.
--
-- Extending the scale to 0 ensures that absolute values are comparable but
-- may obscure differences between values if the variation of the data is small
-- compared to its absolute size.
data IncludeZeroPolicy = IncludeZero | DontIncludeZero

data Continuous a

-- | Create a continuous scale (map between real numbers).
continuous ::
  Extent a -- ^ The source range
  -> IncludeZeroPolicy -- ^ Whether to extend the source range to include 0
  -> ScaleOptions Continuous a a
continuous = ContScale

instance (Ord a, RealFrac a, Floating a) => Scalable Continuous a a where
  type Target Continuous = Identity
  type TargetRange Continuous a = Extent a
  data ScaleOptions Continuous a a = ContScale (Extent a) IncludeZeroPolicy
  scale (ContScale ex zp) tgt = scMap where
    (mn, mx) = toTuple $
      case zp of
        IncludeZero     -> extent 0 <> ex
        DontIncludeZero -> ex
    tgtEx = toTuple tgt
    (tcks, _) = looseLabels defaultLabelCount (mn, mx)
    srcEx = toTuple $ foldMap1 extent tcks
    scMap = Identity . linear srcEx tgtEx

data Cardinal a

-- | Create a cardinal scale (map from integers to 'Double').
cardinal :: Extent a -> ScaleOptions Cardinal a Double
cardinal = CardScaleOptions

mkDouble :: Integral a => a -> Double
mkDouble = fromIntegral

mkA :: Integral a => Double -> a
mkA = fromIntegral . (ceiling :: Double -> Integer)

instance (Ord a, Integral a) => Scalable Cardinal a Double where
  type Target Cardinal = Identity
  type TargetRange Cardinal Double = Extent Double
  data ScaleOptions Cardinal a Double = CardScaleOptions (Extent a)
  scale (CardScaleOptions ex) tgt = scMap where
    (mn, mx) = toTuple ex
    tgtEx = toTuple tgt
    tcks =
      fmap mkA
      $ fst
      $ looseLabels defaultLabelCount (mkDouble mn, mkDouble mx) -- TODO: Write a different version of looseLabels for integers
    srcEx = toTuple $ foldMap1 (extent . mkDouble) tcks
    scMap = dimap mkDouble Identity (linear srcEx tgtEx)

data Ordinal a

-- | Create an ordinal scale (map from some type with an 'Ord' instance to
--   'Extent Double').
ordinal ::
  NonEmpty a -- ^ The domain
  -> (a -> String) -- ^ A description of the values (for creating labels)
  -> ScaleOptions Ordinal a Double
ordinal = OrdScaleOptions

instance (Ord a, Eq a) => Scalable Ordinal a Double where
  type Target Ordinal = Extent
  type TargetRange Ordinal Double = Extent Double
  data ScaleOptions Ordinal a Double = OrdScaleOptions (NonEmpty a) (a -> String)
  scale (OrdScaleOptions ex _) tgt = scMap where
    (f, t) = toTuple tgt
    stepSize = (t - f) / fromIntegral n
    n = length ex
    step i = (f + i * stepSize, f + (i + 1) * stepSize)
    theMap = Map.fromList
      $ zip (NonEmpty.toList ex)
      $ fmap (fromTuple . step) [0 ..]
    scMap av = Map.findWithDefault tgt av theMap

-- | A type that is the result of a binary function
class ProductV p where
  type LeftV p :: *
  type RightV p :: *

instance ProductV Void where
  type LeftV Void = Void
  type RightV Void = Void

instance ProductV (a, b) where
  type LeftV (a, b) = a
  type RightV (a, b) = b

instance ProductV (Either a b) where
  type LeftV (Either a b) = a
  type RightV (Either a b) = b

newtype Product f g a = Product { getProduct :: (f (LeftV a), g (RightV a)) }

-- | The product of two scales.
product :: ScaleOptions f a b -> ScaleOptions g c d -> ScaleOptions (Product f g) (a, c) (b, d)
product = ProdScaleOpts

instance (
  Scalable f a b,
  Scalable g c d) => Scalable (Product f g) (a, c) (b, d) where
    type Target (Product f g) = Product (Target f) (Target g)
    type TargetRange (Product f g) (b, d) = (TargetRange f b, TargetRange g d)
    data ScaleOptions (Product f g) (a, c) (b, d) =
      ProdScaleOpts
        (ScaleOptions f a b)
        (ScaleOptions g c d)
    scale (ProdScaleOpts fa gb) (ltgt, rtgt) = scMap where
      fm = scale fa ltgt
      gm = scale gb rtgt
      scMap (a, c) = Product (fm a, gm c)

data Nested (f :: * -> *) (g :: * -> *) p

-- | 'nested' takes two scales @g@ and @f@ and creates a @g@ scale within each
--   @f@ result. This requires 'Target f', @Target g@ and @TargetRange g@ to be
--  identical.
nested :: ScaleOptions f a b -> ScaleOptions g c b -> ScaleOptions (Nested f g) (a, c) b
nested = NestScaleOpts

instance (
  Target g b ~ TargetRange g b,
  Target f b ~ TargetRange g b,
  Scalable f a b,
  Scalable g c b) => Scalable (Nested f g) (a, c) b where
    type Target (Nested f g) = (Target f)
    type TargetRange (Nested f g) b = TargetRange f b
    data ScaleOptions (Nested f g) (a, c) b =
      NestScaleOpts
        (ScaleOptions f a b)
        (ScaleOptions g c b)
    scale (NestScaleOpts fa gb) ftgt = scMap where
      fm = scale fa ftgt
      scMap (a, c) =
        let fma = fm a
            gm = scale gb fma in
            gm c

data Transformed (f :: * -> *) a

-- | Map the results of a scale monomorphically.
transformed :: (Target f b -> Target f b) -> (ScaleOptions f) a b -> ScaleOptions (Transformed f) a b
transformed = TransformedOpts

instance (
  Target f b ~ h b,
  Functor h,
  Scalable f a b) => Scalable (Transformed f) a b where
    type Target (Transformed f) = Target f
    type TargetRange (Transformed f) b = TargetRange f b
    data ScaleOptions (Transformed f) a b = TransformedOpts (Target f b -> Target f b) ((ScaleOptions f) a b)
    scale (TransformedOpts f o) tgt = f <$> scale o tgt

data Anchored (f :: * -> *) a

-- | Extend a scale by "anchoring" its results to a fixed point of the domain
anchored :: a -> ScaleOptions f a b -> ScaleOptions (Anchored f) a b
anchored = AnchoredOpts

newtype Pair a = Pair { getPair :: (a, a) }

instance (
  Applicative (Target f),
  Scalable f a b) => Scalable (Anchored f) a b where
    type Target (Anchored f) = Compose (Target f) Pair
    type TargetRange (Anchored f) b = TargetRange f b
    data ScaleOptions (Anchored f) a b = AnchoredOpts a (ScaleOptions f a b)
    scale (AnchoredOpts anch opts) tgt x =
      let f = scale opts tgt in
        Compose $ curry Pair <$> f anch <*> f x

-- | Apply two scales to the same value
split :: ScaleOptions f a b -> ScaleOptions g a c -> ScaleOptions (Split f g) a (b, c)
split = SplitScaleOpts

data Split (f :: * -> *) (g :: * -> *) a

instance (
  Scalable f a b,
  Scalable g a c
  ) => Scalable (Split f g) a (b, c) where
    type Target (Split f g) = Product (Target f) (Target g)
    type TargetRange (Split f g) (b, c) = (TargetRange f b, TargetRange g c)
    data ScaleOptions (Split f g) a (b, c) = SplitScaleOpts
      (ScaleOptions f a b)
      (ScaleOptions g a c)
    scale (SplitScaleOpts fa ga) (ltgt, rtgt) = scMap where
      fm = scale fa ltgt
      gm = scale ga rtgt
      scMap a = Product (fm a, gm a)
