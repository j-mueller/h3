module Main where

import           Control.Lens
import           Data.H3.Colour                 (ordinalColours, toCSSColour)
import           Data.H3.Extent                 (Extent, extent, fromTuple,
                                                 resize, toTuple)
import           Data.H3.Scalable               (Scalable (..))
import           Data.H3.Scales                 (IncludeZeroPolicy (..),
                                                 Product (..), continuous)
import           Data.H3.Svg                    (ViewBoxMode (..), renderSvg)
import           Data.H3.Visuals                (ChartVisuals (..),
                                                 FontSize (..),
                                                 LabelOffset (..), Pixel (..),
                                                 Shape (..), Vis (..),
                                                 backgroundShapes, cartesian,
                                                 foregroundShapes, mapColour,
                                                 noGrid)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.Semigroup.Foldable        (Foldable1 (..))
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.IO              as TextIO
import qualified Graphics.Svg                   as SVG
import           Statistics.Distribution        (ContDistr (density),
                                                 Mean (mean), Variance (stdDev))
import qualified Statistics.Distribution.Normal as Distribution.Normal

newtype PlotData = PlotData { getPlots :: [(Double, Double)] }

_PlotData :: Iso' PlotData [(Double, Double)]
_PlotData = iso getPlots PlotData

main :: IO ()
main = do
  let
    plts  = [
      PlotData $ sample 1.0 0.25,
      PlotData $ sample 2.0 0.44]
    dims = (fromTuple (0, 900), fromTuple (500, 0))
    r    = renderSvg (AddViewBox 1.25) (mapColour Text.pack . plotShape plts) dims
  TextIO.writeFile "out.svg" $ SVG.prettyText r

-- | Sample from a normal distribution
sample :: Double -> Double -> [(Double, Double)]
sample theMean theStdDev = vls where
  d = Distribution.Normal.normalDistr theMean theStdDev
  (f, mn, mx) = let m = mean d
                    std = stdDev d in
                    (density d, m - 3 * std, m + 3 * std)
  samples = fmap (\i -> mn + i * delta) [0 .. n] where
    delta = (mx - mn) / n
    n = 100
  vls = fmap (over _2 f) (zip samples samples)

plotShape :: [PlotData] -> (Extent Double, Extent Double) -> Shape String (Pixel Double, Pixel Double)
plotShape ps tgt = result where
  allPoints = foldMap getPlots ps
  result = case allPoints of
    [] -> EmptyShape
    (x:xs) -> over both Pixel <$> AGroup [backgroundShapes s, plotLines, foregroundShapes s] where
      (xExtent, yExtent) = foldMap1 (over both extent) (x :| xs)
      xOpts = continuous xExtent DontIncludeZero
      yOpts = continuous yExtent DontIncludeZero
      tickLength = 5
      ((_, _), (yLw, _)) = over both toTuple tgt
      xyOpts = cartesian
        (fromTuple (yLw, yLw + tickLength))
        (fromTuple (negate tickLength, 0))
        (FontSize "10pt")
        (LabelOffset (-20, 15))
        xOpts
        yOpts
      s = visuals (Vis xyOpts) tgt
      colors = toCSSColour . runIdentity <$> scale (ordinalColours [1 .. length xs]) ()
      plotLines =
        fmap (over both runIdentity . getProduct . combinedScale)
        $ AGroup
        $ mkPlot <$> zip ps [1..]
      mkPlot (PlotData pts, col) = AColouredShape (colors col) $ ALine pts
      combinedScale = scale xyOpts tgt
