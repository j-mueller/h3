{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.H3.Examples.BarChart where

import           Control.Lens
import           Data.Functor.Compose    (Compose (..))
import           Data.Functor.Identity   (Identity (..))
import           Data.H3.Colour          (ordinalColours, toCSSColour)
import           Data.H3.Extent          (Extent, extent, fromTuple, resize,
                                          toTuple)
import           Data.H3.Scalable        (Scalable (..))
import           Data.H3.Scales          (IncludeZeroPolicy (..), Pair (..),
                                          Product (..), anchored, continuous,
                                          nested, ordinal, transformed)
import           Data.H3.Svg             (ViewBoxMode (..), renderSvg)
import           Data.H3.Visuals         (ChartVisuals (..), FontSize (..),
                                          LabelOffset (..), Pixel (..),
                                          Shape (..), Vis (..),
                                          backgroundShapes, cartesian,
                                          foregroundShapes, mapColour, noGrid)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Semigroup.Foldable (Foldable1 (..))
import qualified Data.Text               as Text
import qualified Data.Text.Lazy.IO       as TextIO
import           GHC.Generics            (Generic)
import qualified Graphics.Svg            as SVG

newtype Bar = Bar { getBar :: (String, Double) }
    deriving (Eq, Ord, Show, Generic)

_Bar :: Iso' Bar (String, Double)
_Bar = iso getBar Bar

newtype Bars = Bars { getBars :: NonEmpty (String, (Bar, Bar)) }
  deriving (Eq, Ord, Show, Generic)

_Bars :: Iso' Bars (NonEmpty (String, (Bar, Bar)))
_Bars = iso getBars Bars

plotBarChart :: Bars -> (Extent Double, Extent Double) -> Shape String (Pixel Double, Pixel Double)
plotBarChart bs tgt = over both Pixel <$> AGroup [backgroundShapes s, allBars, foregroundShapes s] where

  -- set up x- and y-axes
  xOpts = noGrid $ nested
    (transformed (resize 0.9) (ordinal outerGroups id))
    (ordinal innerGroups id)
  yOpts = anchored 0 $ continuous yRange IncludeZero

  -- set up the cartesian chart (product of x and y axes with layout config.)
  xyOpts = cartesian
    (fromTuple (yLw, yLw + tickLength))
    (fromTuple (negate tickLength, 0))
    (FontSize "10pt")
    (LabelOffset (-20, 15))
    xOpts
    yOpts
  xyScale = scale xyOpts tgt -- initialise the xy scale with target dimensions

  -- set up a map for colours
  cols = toCSSColour . runIdentity <$> scale (ordinalColours $ NonEmpty.toList innerGroups) ()

  -- mkBar maps a single 'Bar' to a rectangle in target coordinates
  mkBar grp nm dbl =
    let Product (xExt, g) = xyScale ((grp, nm), dbl)
        (Compose (Identity (Pair (ylw, yhg)))) = g
        (xlw, xhg) = toTuple xExt in
    AColouredShape (cols nm)
    $ ARectangle (fromTuple ((xlw, ylw), (xhg, yhg)))
  mkBars grp (Bar (lnm, ldbl), Bar (rnm, rdbl)) =
    AGroup [mkBar grp lnm ldbl, mkBar grp rnm rdbl]

  -- bar shapes
  allBars = AGroup
    $ NonEmpty.toList
    $ uncurry mkBars <$> getBars bs

  -- get the extent of all dimensions from the data
  ((_, _), (yLw, _)) = over both toTuple tgt
  yRange = yl <> yr
  (yl, yr) = foldMap1 (over both (extent . view (_Bar . _2)) . view _2) $ getBars bs
  outerGroups =
    NonEmpty.nub (view _1 <$> view _Bars bs)
  innerGroups =
    NonEmpty.nub
      $ foldMap1 (\(l, r) -> (l :| [r]))
      $ fmap (over both (view (_Bar . _1)) . view _2) (view _Bars bs)

  tickLength = 5
  s = visuals (Vis xyOpts) tgt

barChartExample :: IO ()
barChartExample = do
  let
    brs  = Bars $
            ("2014", (Bar ("a", 100), Bar ("b", 140))) :|
            [("2015", (Bar ("a", 80), Bar ("b", 160))),
             ("2016", (Bar ("a", 70), Bar ("b", 165))),
             ("2017", (Bar ("a", 70), Bar ("b", 165))),
             ("2018", (Bar ("a", 70), Bar ("b", 165)))]
    dims = (fromTuple (0, 900), fromTuple (500, 0))
    r    = renderSvg (AddViewBox 1.1) (mapColour Text.pack . plotBarChart brs) dims
  TextIO.writeFile "out.svg" $ SVG.prettyText r
