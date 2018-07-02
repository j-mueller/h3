{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Data.H3.Geo.Shapefile where

import           Data.H3.Geo.Types              (Point (..), Polygon (..),
                                                 WGS84 (..))

import           Data.List.NonEmpty             (NonEmpty, nonEmpty)
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Data.Semigroup.Foldable        (Foldable1 (..))
import           Geometry.Shapefile.MergeShpDbf (readShpWithDbf)
import           Geometry.Shapefile.ReadShp     (readShpFile)
import           Geometry.Shapefile.Types       (DbfRecord (..),
                                                 RecContents (..), ShpRec (..),
                                                 shpRecs)
import           System.Directory               (doesFileExist)
import           System.FilePath                (dropExtension)

-- | Load a shapefile using the readshp library.
--   TODO: If the shapefile comes with a .prj file then that should be
--         consulted to determine the format of the coordinates
--         (we just assume it's WGS 84 here)
loadShapeFromFile :: FilePath -> IO (Either String (NonEmpty (Polygon WGS84)))
loadShapeFromFile fp = do
  dbfExists <- doesFileExist (dropExtension fp <> ".dbf")
  file <- if dbfExists then readShpWithDbf fp else readShpFile fp
  let x = mapM toShape $ shpRecs file
  return $ x >>= maybe (Left err) (Right . fold1) . nonEmpty where
    err = "The file " ++ fp ++ " did not contain any polygons."

toShape :: ShpRec -> Either String (NonEmpty (Polygon WGS84))
toShape =  maybe (Left "empty shpRecContents") mkShape . shpRecContents

mkShape :: RecContents -> Either String (NonEmpty (Polygon WGS84))
mkShape =
  let f = maybe (Left "No polygon found") (Right . fmap Polygon) . nonEmpty . catMaybes . fmap (nonEmpty . fmap (WGS84 . Point))  in
  \case
    RecPolygon{..}   -> f recPolPoints
    RecPolygonM {..} -> f recPolMPoints
    RecPolygonZ {..} -> f recPolZPoints
    e                -> Left $ "shapefile should contain polygons but contains" ++ take 20 (show e)

toLabels :: ShpRec -> [String]
toLabels = maybe [] (fmap (fromMaybe "" . getLabel)) . shpRecLabel

getLabel :: DbfRecord -> Maybe String
getLabel = \case
  DbfString s -> Just s
  _ -> Nothing
