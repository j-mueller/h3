module Data.H3.Examples.Map where

import           Data.H3.Extent          (Extent, extent, fromTuple, toTuple)
import           Data.H3.Geo.Projection  (albers')
import           Data.H3.Geo.Shapefile   (loadShapeFromFile)
import           Data.H3.Geo.Types       (Point (..), Polygon (..), Radians,
                                          WGS84 (..), toRad)
import           Data.H3.Scalable        (Scalable (..))
import           Data.H3.Svg             (renderSvg')
import           Data.H3.Visuals         (Pixel (..), Shape (..))

import           Data.Bifunctor          (Bifunctor (..))
import           Data.Functor.Identity   (Identity (..))
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Semigroup.Foldable (Foldable1 (..))
import qualified Data.Text.Lazy.IO       as TextIO
import qualified Graphics.Svg            as SVG

mapExample :: IO ()
mapExample = do
  Right shp <- loadShapeFromFile "src\\h3-geo\\shapefiles\\Mannheim\\Mannheim.shp"
  let ext = hlp
            $ foldMap1 (foldMap1 ((\(lng, lt) -> (extent lng, extent lt)) . getPoint . fmap toRad . getWGS84)) shp
      proj = fmap (bimap Pixel Pixel . runIdentity) . scale (albers' ext mannheim)
      mannheim = Point (toRad 8.4660395, toRad 49.4874592)
      shp' = fmap (fmap toRad . getWGS84) <$> shp
      mkLine x = ALine . fmap (proj x) . NonEmpty.toList . getPolygon
      mkGroup x = AGroup $ fmap (mkLine x) (NonEmpty.toList shp')
      dims = (fromTuple (0, 930), fromTuple (1630, 0))
      r = renderSvg' mkGroup dims
  TextIO.writeFile "out.svg" $ SVG.prettyText r

hlp :: (Extent Radians, Extent Radians) -> (Point Radians, Point Radians)
hlp (lnEx, ltEx) = (lw, hg) where
  lw = Point (minLong, minLat)
  hg = Point (maxLong, maxLat)
  (minLong, maxLong) = toTuple lnEx
  (minLat, maxLat)   = toTuple ltEx
