{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.H3.Scales(
  -- * Basic scales
  arrow,
  -- ** Continuous (real numbers)
  continuous,
  Continuous,
  IncludeZeroPolicy(..),
  -- ** Cardinal (integers)
  cardinal,
  Cardinal,
  -- ** Ordinal (Ord a)
  ordinal,
  Ordinal,
  -- * Combinators
  ProductV,
  product,
  Product,
  nested,
  Nested,
  transformed,
  Transformed,
  cartesian,
  Cartesian,
  -- * Visuals
  noGrid,
  NoGrid,
  noVisuals,
  NoVisuals
) where

import           Data.Bifunctor           (Bifunctor (..))
import           Data.Functor.Identity    (Identity (..))
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Map                 as Map
import           Data.Profunctor          (Profunctor (..))
import           Data.Semigroup.Foldable  (Foldable1 (..))
import           Data.String              (IsString (..))
import qualified Data.Text                as Text
import qualified Data.Text.Format.Numbers as F
import           Data.Void                (Void)

import           Data.H3.Extent           (Extent, extent, fromTuple, toTuple)
import           Data.H3.Scalable         (ChartVisuals (..), Scalable (..),
                                           Target, TargetRange,
                                           VisualElements (..), arrow)
import           Data.H3.Shape            (Shape)
import           Data.H3.Utils            (computeMidpoint, defaultLabelCount,
                                           linear, looseLabels)
import           Prelude                  hiding (product)


-- | Indicates whether to extend the source (domain) of a continuous scale to
-- include 0.
--
-- Extending the scale to 0 ensures that absolute values are comparable but
-- may obscure differences between values if the variation of the data is small
-- compared to its absolute size.
data IncludeZeroPolicy = IncludeZero | DontIncludeZero

data Continuous a

type instance Target Continuous = Identity
type instance TargetRange Continuous = Extent

-- | Create a continuous scale (map between real numbers).
continuous ::
  Extent a -- ^ The source range
  -> IncludeZeroPolicy -- ^ Whether to extend the source range to include 0
  -> ScaleOptions Continuous a b
continuous = ContScale

instance (Ord a, RealFrac a, Floating a) => Scalable Continuous a a where
  data ScaleOptions Continuous a b = ContScale (Extent a) IncludeZeroPolicy
  scale (ContScale ex zp) tgt = scMap where
    (mn, mx) = toTuple $
      case zp of
        IncludeZero     -> extent 0 <> ex
        DontIncludeZero -> ex
    tgtEx = toTuple tgt
    (tcks, _) = looseLabels defaultLabelCount (mn, mx)
    srcEx = toTuple $ foldMap1 extent tcks
    scMap = Identity <$> linear srcEx tgtEx

instance (Ord a, RealFrac a, Floating a) => ChartVisuals Continuous a a where
  visuals opts@(ContScale ex zp) tgt = VisualElements{..} where
    (mn, mx) = toTuple $
      case zp of
        IncludeZero     -> extent 0 <> ex
        DontIncludeZero -> ex
    (tcks, nfrac) = looseLabels defaultLabelCount (mn, mx)
    veTicks = runIdentity . scMap <$> NonEmpty.toList tcks
    veGridLines = zip veTicks veTicks
    veAxisLabels = (\n -> (scTickLabel n, runIdentity $ scMap n)) <$> NonEmpty.toList tcks
    veLegend = []
    scMap = scale opts tgt
    scTickLabel = fromString . Text.unpack . F.prettyF cfg where
      cfg = F.PrettyCfg {
        F.pc_decimals = fromIntegral nfrac,
        F.pc_thousandsSep = Just ',',
        F.pc_decimalSep = '.' }

data Cardinal a

type instance Target Cardinal = Identity
type instance TargetRange Cardinal = Extent

-- | Create a cardinal scale (map from integers to 'Double').
cardinal :: Extent a -> ScaleOptions Cardinal a b
cardinal = CardScaleOptions

mkDouble :: Integral a => a -> Double
mkDouble = fromIntegral

mkA :: Integral a => Double -> a
mkA = fromIntegral . (ceiling :: Double -> Integer)

instance (Ord a, Integral a) => Scalable Cardinal a Double where
  data ScaleOptions Cardinal a b = CardScaleOptions (Extent a)
  scale (CardScaleOptions ex) tgt = scMap where
    (mn, mx) = toTuple ex
    tgtEx = toTuple tgt
    tcks =
      fmap mkA
      $ fst
      $ looseLabels defaultLabelCount (mkDouble mn, mkDouble mx) -- TODO: Write a different version of looseLabels for integers
    srcEx =  toTuple $ foldMap1 (extent . mkDouble) tcks
    scMap = dimap mkDouble Identity (linear srcEx tgtEx)

instance (Ord a, Integral a) => ChartVisuals Cardinal a Double where
  visuals o@(CardScaleOptions ex) tgt = scVis where
    (mn, mx) = toTuple ex
    (tcks, nfrac) =
      (\(l, r) -> (fmap mkA l, r))
      $ looseLabels defaultLabelCount (mkDouble mn, mkDouble mx) -- TODO: Write a different version of looseLabels for integers
    theTicks = runIdentity . scMap <$> NonEmpty.toList tcks
    scVis = VisualElements
      theTicks
      (zip theTicks theTicks)
      ((\n -> (scTickLabel n, runIdentity $ scMap n)) <$> NonEmpty.toList tcks)
      []
    scMap = scale o tgt
    scTickLabel = fromString . Text.unpack . F.prettyF cfg . mkDouble where
      cfg = F.PrettyCfg {
        F.pc_decimals = fromIntegral nfrac,
        F.pc_thousandsSep = Just ',',
        F.pc_decimalSep = '.'}

data Ordinal a

type instance Target Ordinal = Extent
type instance TargetRange Ordinal = Extent

-- | Create an ordinal scale (map from some type with an 'Ord' instance to
--   'Extent Double').
ordinal ::
  NonEmpty a -- ^ The domain
  -> (a -> String) -- ^ A description of the values (for creating labels)
  -> ScaleOptions Ordinal a b
ordinal = OrdScaleOptions

instance (Ord a, Eq a) => Scalable Ordinal a Double where
  data ScaleOptions Ordinal a b = OrdScaleOptions (NonEmpty a) (a -> String)
  scale (OrdScaleOptions ex _) tgt = scMap where
    (f, t) = toTuple tgt
    stepSize = (t - f) / fromIntegral n
    n = length ex
    step i = (f + i * stepSize, f + (i + 1) * stepSize)
    theMap = Map.fromList
      $ zip (NonEmpty.toList ex)
      $ fmap (fromTuple . step) [0 ..]
    scMap av = Map.findWithDefault tgt av theMap

instance (Ord a, Eq a) => ChartVisuals Ordinal a Double where
  visuals (OrdScaleOptions ex f) tgt = scVis where
    scMap = scale (OrdScaleOptions ex f) tgt
    theTicks = computeMidpoint . scMap <$> NonEmpty.toList ex
    scVis = VisualElements
      theTicks
      []
      ((\n' -> (f n', computeMidpoint $ scMap n')) <$> NonEmpty.toList ex)
      []

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

instance ProductV p => ProductV (Shape s p) where
  type LeftV (Shape s p) = Shape s (LeftV p)
  type RightV (Shape s p) = Shape s (RightV p)

newtype Product f g a = Product (f (LeftV a), g (RightV a))

type instance Target (Product f g) = Product (Target f) (Target g)
type instance TargetRange (Product f g) = Product (TargetRange f) (TargetRange g)

-- | The product of two scales.
product :: ScaleOptions f a b -> ScaleOptions g c d -> ScaleOptions (Product f g) (a, c) (b, d)
product = ProdScaleOpts

instance (
  Scalable f a b,
  Scalable g c d) => Scalable (Product f g) (a, c) (b, d) where
    data ScaleOptions (Product f g) p q =
      ProdScaleOpts
        (ScaleOptions f (LeftV p) (LeftV q))
        (ScaleOptions g (RightV p) (RightV q))
    scale (ProdScaleOpts fa gb) (Product (ltgt, rtgt)) = scMap where
      fm = scale fa ltgt
      gm = scale gb rtgt
      scMap (a, c) = Product (fm a, gm c)

data Nested (f :: * -> *) (g :: * -> *) p

type instance Target (Nested f g) = (Target f)
type instance TargetRange (Nested f g) = TargetRange f

-- | 'nested' takes two scales @g@ and @f@ and creates a @g@ scale within each
--   @f@ result. This requires 'Target f', @Target g@ and @TargetRange g@ to be
--  identical.
nested :: ScaleOptions f a b -> ScaleOptions g c b -> ScaleOptions (Nested f g) (a, c) b
nested = NestScaleOpts

instance (
  Target g ~ TargetRange g,
  Target f ~ TargetRange g,
  Scalable f a b,
  Scalable g c b) => Scalable (Nested f g) (a, c) b where
    data ScaleOptions (Nested f g) p q =
      NestScaleOpts
        (ScaleOptions f (LeftV p) q)
        (ScaleOptions g (RightV p) q)
    scale (NestScaleOpts fa gb) ftgt = scMap where
      fm = scale fa ftgt
      scMap (a, c) =
        let fma = fm a
            gm = scale gb fma in
            gm c

instance (
  Target g ~ TargetRange g,
  Target f ~ TargetRange g,
  Scalable f a b,
  Scalable g c b,
  ChartVisuals f a b)
  => ChartVisuals (Nested f g) (a, c) b where
    visuals (NestScaleOpts fa _) = visuals fa

data Transformed (f :: * -> *) a

type instance Target (Transformed f) = Target f
type instance TargetRange (Transformed f) = TargetRange f

-- | Map the results of a scale monomorphically.
transformed :: (Target f b -> Target f b) -> (ScaleOptions f) a b -> ScaleOptions (Transformed f) a b
transformed = TransformedOpts

instance (
  Functor (Target f),
  Scalable f a b) => Scalable (Transformed f) a b where
  data ScaleOptions (Transformed f) a b = TransformedOpts (Target f b -> Target f b) ((ScaleOptions f) a b)
  scale (TransformedOpts f o) tgt = f <$> scale o tgt

instance (
  Functor (Target f),
  Scalable f a b,
  ChartVisuals f a b)
  => ChartVisuals (Transformed f) a b where
    visuals (TransformedOpts _ o) = visuals o

data NoGrid (f :: * -> *) a

type instance Target (NoGrid f) = Target f
type instance TargetRange (NoGrid f) = TargetRange f

noGrid :: ScaleOptions f a b -> ScaleOptions (NoGrid f) a b
noGrid = NoGrid

instance Scalable f a b => Scalable (NoGrid f) a b where
  data ScaleOptions (NoGrid f) a b = NoGrid ((ScaleOptions f) a b)
  scale (NoGrid opts) = scale opts

instance (
  Scalable f a b,
  ChartVisuals f a b)
  => ChartVisuals (NoGrid f) a b where
    visuals (NoGrid o) tgt = VisualElements tcks [] lbl lgd where
      VisualElements tcks _ lbl lgd = visuals o tgt

data NoVisuals (f :: * -> *) a

type instance Target (NoVisuals f) = Target f
type instance TargetRange (NoVisuals f) = TargetRange f

noVisuals :: ScaleOptions f a b -> ScaleOptions (NoVisuals f) a b
noVisuals = NoVisuals

instance Scalable f a b => Scalable (NoVisuals f) a b where
  data ScaleOptions (NoVisuals f) a b = NoVisuals ((ScaleOptions f) a b)
  scale (NoVisuals opts) = scale opts

instance Scalable f a b => ChartVisuals (NoVisuals f) a b where
  visuals _ _ = mempty

data Cartesian (f :: * -> *) (g :: * -> *) a

type instance Target (Cartesian f g) = Product (Target f) (Target g)
type instance TargetRange (Cartesian f g) = Product (TargetRange f) (TargetRange g)

cartesian :: ScaleOptions f a b -> ScaleOptions g c d -> ScaleOptions (Cartesian f g) (a, c) (b, d)
cartesian = CardScaleOpts

instance (Scalable f a b, Scalable g c d) => Scalable (Cartesian f g) (a, c) (b, d) where
  data ScaleOptions (Cartesian f g) p q =
    CardScaleOpts
      (ScaleOptions f (LeftV p) (LeftV q))
      (ScaleOptions g (RightV p) (RightV q))
  scale (CardScaleOpts fa gb) (Product (ltgt, rtgt)) = scMap where
    fm = scale fa ltgt
    gm = scale gb rtgt
    scMap (a, c) = Product (fm a, gm c)

instance (
  Scalable f a b,
  Scalable g c d,
  ChartVisuals f a b,
  TargetRange f ~ Extent,
  TargetRange g ~ Extent,
  ChartVisuals g c d)
  => ChartVisuals (Cartesian f g) (a, c) (b, d) where
    visuals (CardScaleOpts fo go) (Product (ltgt, rtgt)) = VisualElements t g a l where
      VisualElements ft fg fa fl = visuals fo ltgt
      VisualElements gt gg ga gl = visuals go rtgt
      t = ft' ++ gt'
      g = fg' ++ gg'
      a = fa' ++ ga'
      l = fl' ++ gl'
      (minB, maxB) = toTuple ltgt
      (minD, maxD) = toTuple rtgt
      placeLeft = (minB,)
      placeRight = (maxB,)
      placeTop = (,maxD)
      placeBottom = (,minD)
      ft' = placeBottom <$> ft
      gt' = placeLeft <$> gt
      fg' = bimap placeBottom placeTop <$> fg
      gg' = bimap placeLeft placeRight <$> gg
      fa' = fmap placeBottom <$> fa
      ga' = fmap placeLeft <$> ga
      fl' = fmap placeBottom <$> fl
      gl' = fmap placeLeft <$> gl




