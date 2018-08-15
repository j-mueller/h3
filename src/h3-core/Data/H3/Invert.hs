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
-- Invertible scales ( \( f ^{-1}\) )
--------------------------------------------------------------------
module Data.H3.Invert(
  Invertible(..)
) where

import           Control.Monad           ((>=>))
import           Data.Functor.Compose
import           Data.Functor.Identity
import qualified Data.IntervalMap        as IM
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Profunctor         (Profunctor (..))
import           Data.Semigroup          ((<>))
import           Data.Semigroup.Foldable (Foldable1 (..))

import           Data.H3.Extent          (extent, fromTuple, toTuple)
import           Data.H3.Scalable        (Scalable (..), ScaleOptions)
import           Data.H3.Scales          (Anchored, Cardinal, Continuous,
                                          IncludeZeroPolicy (..), Nested,
                                          Ordinal, Pair (..), Product (..),
                                          ScaleOptions (..), Transformed)
import           Data.H3.Utils           (computeMidpoint, defaultLabelCount,
                                          linear, looseLabels)

-- | The class of scales that can be inverted.
class Scalable f a b => Invertible f a b where

  -- | Assuming fixed scale options and target range, and given an 'a' that is
  --   in the domain of the scale, Just a == invert (scale a).
  invert :: (ScaleOptions f) a b -> TargetRange f b -> (Target f) b -> Maybe a

instance (Ord a, RealFrac a, Floating a) => Invertible Continuous a a where
  invert (ContScale ex zp) tgt (Identity y) = if y >= tgtMin && y <= tgtMax then Just $ f y else Nothing  where
    (mn, mx) = toTuple $
      case zp of
        IncludeZero     -> extent 0 <> ex
        DontIncludeZero -> ex
    tgtEx@(tgtMin, tgtMax) = toTuple tgt
    (tcks, _) = looseLabels defaultLabelCount (mn, mx)
    srcEx = toTuple $ foldMap1 extent tcks
    f = linear tgtEx srcEx

instance (Ord a, Integral a) => Invertible Cardinal a Double where
  invert (CardScaleOptions ex) tgt (Identity y) = if y >= tgtMin && y <= tgtMax then Just $ f y else Nothing where
    (mn, mx) = toTuple ex
    tgtEx@(tgtMin, tgtMax) = toTuple tgt
    tcks =
      fmap mkA
      $ fst
      $ looseLabels defaultLabelCount (mkDouble mn, mkDouble mx) -- TODO: Write a different version of looseLabels for integers
    srcEx = toTuple $ foldMap1 (extent . mkDouble) tcks
    f = dimap id mkA (linear srcEx tgtEx)

mkDouble :: Integral a => a -> Double
mkDouble = fromIntegral

mkA :: Integral a => Double -> a
mkA = fromIntegral . (ceiling :: Double -> Integer)

instance (Ord a, Eq a) => Invertible Ordinal a Double where
  invert (OrdScaleOptions ex _) tgt = f' where
    (f, t) = toTuple tgt
    stepSize = (t - f) / fromIntegral n
    n = length ex
    step i = (f + i * stepSize, f + (i + 1) * stepSize)
    im = IM.fromList
      $ flip zip (NonEmpty.toList ex)
      $ fmap (fromTuple . step) [0 ..]
    f' i = case IM.toList $ IM.containing im $ computeMidpoint i of
      [(_, x)] -> Just x
      _        -> Nothing

instance (
  Invertible f a b,
  Invertible g c d)
  => Invertible (Product f g) (a, c) (b, d) where
    invert (ProdScaleOpts fa gb) (ltgt, rtgt) (Product (l, r)) = (,) <$> l' <*> r' where
      l' = invert fa ltgt l
      r' = invert gb rtgt r

instance (
  Target g b ~ TargetRange g b,
  Target f b ~ TargetRange g b,
  Invertible f a b,
  Invertible g c b
  ) => Invertible (Nested f g) (a, c) b where
    invert (NestScaleOpts fa gb) ftgt i =
      let ia = invert fa ftgt
          sa = scale fa ftgt in
      do
      fm' <- ia i
      fm'' <- invert gb (sa fm') i
      return (fm', fm'')

instance (
  Target f b ~ h b,
  Functor h,
  Invertible f a b) => Invertible (Transformed f) a b where
    invert (TransformedOpts _ i o) tgt = i >=> invert o tgt

instance (
  Traversable (Target f),
  Applicative (Target f),
  Invertible f a b) => Invertible (Anchored f) a b where
    invert (AnchoredOpts _ opts) tgt =
      invert opts tgt . fmap (snd . getPair) . getCompose
