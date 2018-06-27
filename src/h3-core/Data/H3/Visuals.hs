{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
--------------------------------------------------------------------
-- |
-- Copyright : (c) Jann Müller 2018
-- License   :  MIT
-- Maintainer:  Jann Müller <j.mueller.11@alumni.ucl.ac.uk>
-- Stability :  experimental
-- Portability: non-portable
--
-- Shapes that make up charts.
--------------------------------------------------------------------
module Data.H3.Visuals(
  -- * Shapes
  Shape(..),
  mapColour,
  TextAnchor(..),
  FontSize(..),
  _FontSize,
  FontWeight(..),
  _FontWeight,
  Pixel(..),
  _Pixel,
  LabelOffset(..),
  _LabelOffset,
  VisualElements(..),
  backgroundShapes,
  foregroundShapes,
  -- * Scales that generate visual elements
  ChartVisuals(..),
  Vis(..),
  -- * Modifiers for visuals
  noGrid,
  NoGrid,
  noVisuals,
  NoVisuals,
  cartesian,
  Cartesian,
  -- * Constructors
  ScaleOptions(..)
) where

import           Data.Bifunctor           (Bifunctor (..))
import           Data.Functor.Identity    (Identity (..))
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.Profunctor          (Profunctor (..))
import           Data.String              (IsString (..))
import qualified Data.Text                as Text
import qualified Data.Text.Format.Numbers as F
import           GHC.Generics             (Generic)

import           Data.H3.Extent           (Extent, extent, toTuple)
import           Data.H3.Scalable         (Scalable (..), ScaleOptions)
import           Data.H3.Scales           (Anchored, Cardinal, Continuous,
                                           IncludeZeroPolicy (..), Nested,
                                           Ordinal, Product (..),
                                           ScaleOptions (..), Transformed)
import           Data.H3.Utils            (computeMidpoint, defaultLabelCount,
                                           looseLabels)

-- | Indicates where a text element should be anchored, that is where its
--   origin is placed relative to the text.
data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- | Font size
newtype FontSize s = FontSize { getFontSize :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

-- | Font size iso
_FontSize :: forall p f a b. (Profunctor p, Functor f) => p (FontSize a) (f (FontSize b)) -> p a (f b)
_FontSize = dimap FontSize (fmap getFontSize)

-- | Font weight
newtype FontWeight s = FontWeight { getFontWeight :: s }
  deriving (Functor, Eq, Ord, Show, Generic)

-- | Font weight iso
_FontWeight :: forall p f a b. (Profunctor p, Functor f) => p (FontWeight a) (f (FontWeight b)) -> p a (f b)
_FontWeight = dimap FontWeight (fmap getFontWeight)

-- | Pixel unit in the target coordinate system
newtype Pixel a = Pixel { getPixel :: a }
    deriving (Functor, Eq, Ord, Show, Num, Floating, Real, Fractional, RealFrac)

-- | Pixel iso
_Pixel :: forall p f a b. (Profunctor p, Functor f) => p (Pixel a) (f (Pixel b)) -> p a (f b)
_Pixel = dimap Pixel (fmap getPixel)

newtype LabelOffset a = LabelOffset { getLabelOffset :: a }
  deriving (Functor, Eq, Ord, Show, Num, Floating, Real, Fractional, RealFrac)

-- | LabelOffset iso
_LabelOffset :: forall p f a b. (Profunctor p, Functor f) => p (LabelOffset a) (f (LabelOffset b)) -> p a (f b)
_LabelOffset = dimap LabelOffset (fmap getLabelOffset)

-- | Shapes that make up charts. @s@ is the type of colours, and @n@ the type
--  of coordinates. For a 2D chart @n@ should be something like @(Double,
--  Double)@.
data Shape s n =
  ALine [n] -- ^ An open line of several points
  | ARectangle !(Extent n) -- ^ A rectangle, defined by its lower left and upper right corners (if @n ~ (Double, Double)@)
  | AnArea [n] -- ^ An area (closed line)
  | AColouredShape s !(Shape s n) -- ^ Set the colour of a shape
  | AGroup [Shape s n] -- ^ Group several shapes together
  | ALabel !TextAnchor !(FontSize String) !(FontWeight String) !String n -- ^ A label
  | AnOpacity Double !(Shape s n) -- ^ Set the opacity of a shape
  | EmptyShape -- ^ Empty shape
  deriving (Functor, Foldable, Traversable, Show)

-- | Change the colours of a 'Shape'.
mapColour :: (s -> t) -> Shape s n -> Shape t n
mapColour f = \case
  ALine ns -> ALine ns
  ARectangle e -> ARectangle e
  AnArea ns -> AnArea ns
  AColouredShape c sh -> AColouredShape (f c) (mapColour f sh)
  AGroup sh -> AGroup (fmap (mapColour f) sh)
  ALabel ta fs fw s n -> ALabel ta fs fw s n
  AnOpacity d sh -> AnOpacity d (mapColour f sh)
  EmptyShape -> EmptyShape

-- | A collection of scale metadata that needs to be visualised. @n@ is the
-- type of coordinates in the target coordinate system, to allow combinators
-- such as 'Product' and 'Cartesian' to adjust the locations of the elements.
data VisualElements s n = VisualElements {
  veTicks      :: [n], -- ^ Points in the target interval that should be indicated to the user,
  veAxes       :: [(n, n)], -- ^ Start and end points of axes
  veGridLines  :: [(n, n)], -- ^ Start and end points of grid lines
  veAxisLabels :: [(String, n)], -- ^ Points in the target interval that should be labelled
  veLegend     :: [Shape s n] -- ^ Additional data
} deriving (Functor, Show)

instance Semigroup (VisualElements s n) where
  (VisualElements lt lx lg la ll) <> (VisualElements rt rx rg ra rl) =
    VisualElements (lt <> rt) (lx <> rx) (lg <> rg) (la <> ra) (ll <> rl)

instance Monoid (VisualElements s n) where
  mappend = (<>)
  mempty = VisualElements [] [] [] [] []

backgroundShapes :: VisualElements s n -> Shape String n
backgroundShapes (VisualElements _ _ lns _ _) =
  AColouredShape "black"
   $ AnOpacity 0.2
   $ AGroup $ mkLine <$> lns where
    mkLine (x1, x2) = ALine [x1, x2]

foregroundShapes ::
  VisualElements String (x, y)
  -> Shape String (x, y)
foregroundShapes (VisualElements _ lns _ _ lgnd) = AGroup [grd, AGroup lgnd] where
  mkLine (x1, x2) = ALine [x1, x2]
  grd = AColouredShape "black"
        $ AnOpacity 1.0
        $ AGroup
        $ mkLine <$> lns

-- | The class of scales that have metadata in the form of 'VisualElements'.
class ChartVisuals c a b where
  visuals :: c -> a ->  VisualElements String b

newtype Vis a = Vis a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data NoGrid (f :: * -> *) a

noGrid :: ScaleOptions f a b -> ScaleOptions (NoGrid f) a b
noGrid = NoGrid

instance Scalable f a b => Scalable (NoGrid f) a b where
  type Target (NoGrid f) = Target f
  type TargetRange (NoGrid f) b = TargetRange f b
  data ScaleOptions (NoGrid f) a b = NoGrid ((ScaleOptions f) a b)
  scale (NoGrid opts) = scale opts

data NoVisuals (f :: * -> *) a

noVisuals :: ScaleOptions f a b -> ScaleOptions (NoVisuals f) a b
noVisuals = NoVisuals

instance Scalable f a b => Scalable (NoVisuals f) a b where
  type Target (NoVisuals f) = Target f
  type TargetRange (NoVisuals f) b = TargetRange f b
  data ScaleOptions (NoVisuals f) a b = NoVisuals ((ScaleOptions f) a b)
  scale (NoVisuals opts) = scale opts

instance (Ord a, RealFrac a, Floating a) => ChartVisuals (Vis (ScaleOptions Continuous a a)) (Extent a) a where
  visuals (Vis opts@(ContScale ex zp)) tgt = VisualElements{..} where
        (mn, mx) = toTuple $
          case zp of
            IncludeZero     -> extent 0 <> ex
            DontIncludeZero -> ex
        (tcks, nfrac) = looseLabels defaultLabelCount (mn, mx)
        ts = runIdentity . scMap <$> NonEmpty.toList tcks
        veTicks = ts
        veGridLines = zip ts ts
        veAxes = take 1 veGridLines
        veAxisLabels = (\n -> (scTickLabel n, runIdentity $ scMap n)) <$> NonEmpty.toList tcks
        veLegend = []
        scMap = scale opts tgt
        scTickLabel = fromString . Text.unpack . F.prettyF cfg where
          cfg = F.PrettyCfg {
            F.pc_decimals = fromIntegral nfrac,
            F.pc_thousandsSep = Just ',',
            F.pc_decimalSep = '.' }

instance (Ord a, Integral a) => ChartVisuals (Vis (ScaleOptions Cardinal a Double)) (Extent Double) Double where
  visuals (Vis o@(CardScaleOptions ex)) tgt = scVis where
    (mn, mx) = toTuple ex
    (tcks, nfrac) =
      (\(l, r) -> (fmap mkA l, r))
      $ looseLabels defaultLabelCount (mkDouble mn, mkDouble mx) -- TODO: Write a different version of looseLabels for integers
    theTicks = zip ts ts
    ts = runIdentity . scMap <$> NonEmpty.toList tcks
    scVis = VisualElements
      ts
      (take 1 theTicks)
      theTicks
      ((\n -> (scTickLabel n, runIdentity $ scMap n)) <$> NonEmpty.toList tcks)
      []
    scMap = scale o tgt
    scTickLabel = fromString . Text.unpack . F.prettyF cfg . mkDouble where
      cfg = F.PrettyCfg {
        F.pc_decimals = fromIntegral nfrac,
        F.pc_thousandsSep = Just ',',
        F.pc_decimalSep = '.'}

mkDouble :: Integral a => a -> Double
mkDouble = fromIntegral

mkA :: Integral a => Double -> a
mkA = fromIntegral . (ceiling :: Double -> Integer)

instance (Ord a, Eq a)
  => ChartVisuals (Vis (ScaleOptions Ordinal a Double)) (Extent Double) Double where
  visuals (Vis (OrdScaleOptions ex f)) tgt = scVis where
    scMap = scale (OrdScaleOptions ex f) tgt
    ts = computeMidpoint . scMap <$> NonEmpty.toList ex
    scVis = VisualElements
      ts
      []
      []
      ((\n' -> (f n', computeMidpoint $ scMap n')) <$> NonEmpty.toList ex)
      []

instance (
  h ~ TargetRange g b,
  ChartVisuals (Vis (ScaleOptions f a b)) h b)
  => ChartVisuals (Vis (ScaleOptions (Nested f g) (a, c) b)) h b where
    visuals (Vis (NestScaleOpts fa _)) = visuals (Vis fa)

instance (
  h ~ TargetRange f b,
  ChartVisuals (Vis (ScaleOptions f a b)) h b)
  => ChartVisuals (Vis (ScaleOptions (NoGrid f) a b)) h b where
    visuals (Vis (NoGrid o)) tgt = VisualElements tcks ax [] lbl lgd where
      VisualElements tcks ax _ lbl lgd = visuals (Vis o) tgt

instance (
  h ~ TargetRange f b,
  Scalable f a b)
  => ChartVisuals (Vis (ScaleOptions (NoVisuals f) a b)) h b where
  visuals _ _ = mempty

instance (
  h ~ TargetRange f b,
  ChartVisuals (Vis (ScaleOptions f a b)) h b)
  => ChartVisuals (Vis (ScaleOptions (Transformed f) a b)) h b where
    visuals (Vis (TransformedOpts _ o)) = visuals (Vis o)

instance (
  h ~ TargetRange f b,
  ChartVisuals (Vis (ScaleOptions f a b)) h b)
  => ChartVisuals (Vis (ScaleOptions (Anchored f) a b)) h b where
    visuals (Vis (AnchoredOpts _ a)) = visuals $ Vis a

data Cartesian (f :: * -> *) (g :: * -> *) a

cartesian ::
     Extent d
  -> Extent b
  -> FontSize String
  -> LabelOffset (b, d)
  -> ScaleOptions f a b
  -> ScaleOptions g c d
  -> ScaleOptions (Cartesian f g) (a, c) (b, d)
cartesian = CardScaleOpts

instance (Scalable f a b, Scalable g c d) => Scalable (Cartesian f g) (a, c) (b, d) where
  type Target (Cartesian f g) = Product (Target f) (Target g)
  type TargetRange (Cartesian f g) (b, d) = (TargetRange f b, TargetRange g d)
  data ScaleOptions (Cartesian f g) (a, c) (b, d) =
    CardScaleOpts {
      fTickLength :: Extent d,
      gTickLength :: Extent b,
      lblFontSize :: FontSize String,
      lblOffset :: LabelOffset (b, d),
      fOptions :: ScaleOptions f a b,
      gOptions :: ScaleOptions g c d
    }
  scale (CardScaleOpts _ _ _ _ fa gb) (ltgt, rtgt) = scMap where
    fm = scale fa ltgt
    gm = scale gb rtgt
    scMap (a, c) = Product (fm a, gm c)

instance (
  TargetRange f b ~ h b,
  TargetRange g d ~ h d,
  h ~ Extent,
  Num d,
  Num b,
  ChartVisuals (Vis (ScaleOptions g c d)) (h d) d,
  ChartVisuals (Vis (ScaleOptions f a b)) (h b) b)
  => ChartVisuals (Vis (ScaleOptions (Cartesian f g) (a, c) (b, d))) (h b, h d) (b, d) where
    visuals (Vis (CardScaleOpts fTL gTL fs lo fo go)) p = VisualElements [] x g [] l where
      LabelOffset (bOff, dOff) = lo
      (ltgt, rtgt) = p
      VisualElements ft fx fg fa fl = visuals (Vis fo) ltgt
      VisualElements gt gx gg ga gl = visuals (Vis go) rtgt
      t = []
      g = fg' ++ gg'
      x = fx' ++ gx' ++ ft' ++ gt'
      l = fl' ++ gl' ++ flbls ++ glbls
      (fTickL, fTickH) = toTuple fTL
      (gTickL, gTickH) = toTuple gTL
      (minB, maxB) = toTuple ltgt
      (minD, maxD) = toTuple rtgt
      placeLeft = (minB,)
      placeRight = (maxB,)
      placeTop = (,maxD)
      placeBottom = (,minD)
      ft' = fmap (\x -> ((x, fTickL), (x, fTickH))) ft
      gt' = fmap (\y -> ((gTickL, y), (gTickH, y))) gt
      fg' = bimap placeBottom placeTop <$> fg
      fx' = bimap placeBottom placeTop <$> fx
      gg' = bimap placeLeft placeRight <$> gg
      gx' = bimap placeLeft placeRight <$> gx
      fl' = fmap placeBottom <$> fl
      gl' = fmap placeLeft <$> gl
      flbls = makeLabel AnchorMiddle <$> fa
      glbls = makeLabel' AnchorEnd <$> ga
      makeLabel anchor (txt, co) = ALabel anchor fs (FontWeight "200") txt (co, fTickH + dOff)
      makeLabel' anchor (txt, co) = ALabel anchor fs (FontWeight "200") txt (gTickH + bOff, co)
