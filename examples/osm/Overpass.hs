{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple bindings to the "Element" type returned by  Overpass API
--    used by OpenStreetMap.
-- https://wiki.openstreetmap.org/wiki/Elements
module Overpass where

import           Control.Lens           hiding (elements)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types       (parseMaybe)
import qualified Data.ByteString.Char8  as B
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           GHC.Generics
import           Network.HTTP.Req

newtype NodeId = NodeId { getNodeId :: Integer }
  deriving (Eq, Ord, Show)

instance FromJSON NodeId where
  parseJSON = fmap NodeId <$> parseJSON

data OverpassNode = OverpassNode {
  nodeId   :: NodeId,
  nodeLat  :: Double,
  nodeLon  :: Double,
  nodeTags :: Map Text Text
  } deriving (Show, Generic)

instance FromJSON OverpassNode where
  parseJSON = withObject "node" $ \e ->
    OverpassNode
      <$> e .: "id"
      <*> e .: "lat"
      <*> e .: "lon"
      <*> (fromMaybe Map.empty <$> e .:? "tags")

newtype WayId = WayId { getWayId :: Integer }
  deriving (Eq, Ord, Show)

instance FromJSON WayId where
  parseJSON = fmap WayId <$> parseJSON

data OverpassWay i = OverpassWay {
  wayId    :: WayId,
  wayNodes :: [i],
  wayTags  :: Map Text Text
  } deriving (Show, Generic, Functor, Foldable, Traversable)

instance FromJSON i => FromJSON (OverpassWay i) where
  parseJSON = withObject "way" $ \e ->
    OverpassWay
      <$> e .: "id"
      <*> e .: "nodes"
      <*> (fromMaybe Map.empty <$> e .:? "tags")

data Element =
  Node OverpassNode
  | Way (OverpassWay NodeId)
  | Relation Object
  deriving (Show, Generic)

_Node :: Prism' Element OverpassNode
_Node = prism Node $ \case
  Node on -> Right on
  e -> Left e

_Way :: Prism' Element (OverpassWay NodeId)
_Way = prism Way $ \case
  Way w -> Right w
  e -> Left e

instance FromJSON Element where
  parseJSON = withObject "Element" $ \e -> do
    tp :: String <- e .: "type"
    case tp of
      "node"     -> Node <$> parseJSON (Object e)
      "way"      -> Way  <$> parseJSON (Object e)
      "relation" -> pure $ Relation e
      _          -> fail "expected one of node, way, relation"

data OverpassResponse = OverpassResponse
  { version   :: Double
  , generator :: Text
  , osm3s     :: Value
  , elements  :: [Element]
  } deriving (Show, Generic)

instance FromJSON OverpassResponse

elms :: Lens' OverpassResponse [Element]
elms = lens g s where
  g = elements
  s or el = or { elements = el }
