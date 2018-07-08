{-# LANGUAGE OverloadedStrings #-}

module Main (main, genMap) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor          (Bifunctor (..))
import qualified Data.ByteString.Char8   as B
import           Data.Default.Class
import           Data.Functor.Identity   (Identity (..))
import           Data.H3.Extent          (Extent, extent, fromTuple, toTuple)
import           Data.H3.Geo.Projection  (greenwich, mapScale, mercator)
import           Data.H3.Geo.Shapefile   (loadShapeFromFile)
import           Data.H3.Geo.Types       (Degrees (..), Point (..),
                                          Polygon (..), Radians, WGS84 (..),
                                          toRad)
import           Data.H3.Scalable        (Scalable (..))
import           Data.H3.Svg             (renderSvg')
import           Data.H3.Visuals         (Pixel (..), Shape (..))
import qualified Data.List.NonEmpty      as NonEmpty
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Data.Monoid             ((<>))
import           Data.Semigroup.Foldable (Foldable1 (..))
import           Data.Text               (Text)
import qualified Data.Text.Lazy.IO       as TextIO
import           GHC.Generics
import qualified Graphics.Svg            as SVG
import           Network.HTTP.Req
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))

import           Overpass

-- | Runs a query from a file (command line argument) on the overpass-api.de
--   and renders the result as to out.svg
main :: IO ()
main = getArgs >>= genMap . head

-- | Run a query and generate an SVG from the paths it returns
genMap :: FilePath -> IO ()
genMap fp = do
  rsp <- makeRequest fp

  -- the Overpass API response contains a list of nodes and a list of ways,
  -- referencing nodes by IDs. The equations for nodes and ways show how
  -- to convert that to a list of Polygons.
  let nodes = Map.fromList
        $ fmap (\n -> (nodeId n, n))
        $ rsp ^.. elms . folded . _Node
      Just ways =
        fmap
          (NonEmpty.fromList . fmap (Polygon . NonEmpty.fromList . wayNodes . fmap getCoords))
        $ traverse (traverse $ flip Map.lookup nodes)
        $ rsp ^.. elms . folded . _Way

      -- The rest of the code generates the SVG and is almost identical
      -- to the map example.
      ext = hlp
        $ foldMap1 (foldMap1 ((\(lng, lt) -> (extent lng, extent lt)) . getPoint . fmap toRad . getWGS84)) ways
      proj = fmap (bimap Pixel Pixel . runIdentity) . scale opts
      opts = mapScale ext $ mercator greenwich
      shp' = fmap (fmap toRad . getWGS84) <$> ways
      mkLine x = ALine . fmap (proj x) . NonEmpty.toList . getPolygon
      mkGroup x = AGroup $ fmap (mkLine x) (NonEmpty.toList shp')
      dims = (fromTuple (0, 830), fromTuple (660, 0))
      r = renderSvg' mkGroup dims
  TextIO.writeFile "out.svg" $ SVG.prettyText r

makeRequest :: FilePath -> IO OverpassResponse
makeRequest fp = runReq def $ do
  v <- req POST (https "overpass-api.de" /: "api" /: "interpreter") (ReqBodyFile fp) jsonResponse mempty
  let rsp = responseBody v :: OverpassResponse
  return rsp

getCoords :: OverpassNode -> WGS84
getCoords on = WGS84 $ Point (Degrees $ nodeLon on, Degrees $ nodeLat on)

hlp :: (Extent Radians, Extent Radians) -> (Point Radians, Point Radians)
hlp (lnEx, ltEx) = (lw, hg) where
  lw = Point (minLong, minLat)
  hg = Point (maxLong, maxLat)
  (minLong, maxLong) = toTuple lnEx
  (minLat, maxLat)   = toTuple ltEx
