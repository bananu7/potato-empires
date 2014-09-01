{-# LANGUAGE OverloadedStrings #-}
module Potato.Web.Serialization where
import Data.Aeson.Types
import Potato.Game
import Potato.Web.Types
import Data.Maybe
import Data.HashMap.Strict (union)
import Control.Applicative
import Control.Monad

unionObjects (Object a) (Object b) = a `union` b
combinePairs :: (ToJSON a, ToJSON b) => [(a,b)] -> [Object]
combinePairs = map (\(a,b) -> (toJSON a) `unionObjects` (toJSON b))

instance ToJSON InitialStatePacket where
    toJSON (InitialStatePacket fields units cities timestamp) = object [
              "map" .= fields,
              "cities" .= combinePairs cities,
              "units" .= combinePairs units,
              "timestamp" .= timestamp]

instance Show MapField where
    show (MapField f u c) = fs ++ us ++ cs
        where fs = show f
              us = if isJust u then show (fromJust u)
                               else ""
              cs = if isJust c then show (fromJust c)
                               else ""

instance ToJSON MapField where
    toJSON = toJSON . show

instance ToJSON FieldType where
    toJSON f | f == Land = String "grass"
             | f == Water = String "water" 

instance ToJSON Player where
    toJSON = toJSON . show

instance ToJSON City where
    toJSON (City name) = object ["name" .= name]

instance ToJSON Unit where
    toJSON (Unit value owner) = object ["value" .= value, "owner" .= owner]

instance ToJSON Point where
    toJSON (Point x y) = object ["x" .= x, "y" .= y]
instance FromJSON Point where
    parseJSON (Object v) = Point <$>
                      v .: "x" <*>
                      v .: "y"
    parseJSON _          = mzero


instance ToJSON MovePacket where
  toJSON (MovePacket from to) = object ["from" .= from, "to" .= to]
instance FromJSON MovePacket where
    parseJSON (Object v) = MovePacket <$>
                      v .: "from" <*>
                      v .: "to"
    parseJSON _          = mzero
