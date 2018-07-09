{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8  as B
import           Data.Default.Class
import           Data.Map               (Map)
import           Data.Maybe             (fromJust)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Req

data OverpassResponse = OverpassResponse
  { version   :: Double
  , generator :: Text
  , osm3s     :: Value
  , elements  :: [OverpassNode]
  } deriving (Show, Generic)

instance FromJSON OverpassResponse

data OverpassNode = OverpassNode {
  id   :: Integer,
  lat  :: Double,
  lon  :: Double,
  tags :: Map Text Text
  } deriving (Show, Generic)

instance FromJSON OverpassNode

-- node, way, relation https://wiki.openstreetmap.org/wiki/Elements


main :: IO ()
main = runReq def $ do
  v <- req POST (https "overpass-api.de" /: "api" /: "interpreter") (ReqBodyFile "query.txt") jsonResponse mempty
  liftIO $ print (responseBody v :: OverpassResponse)
