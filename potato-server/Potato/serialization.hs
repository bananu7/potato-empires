module Potato.Serialization where
import Data.Aeson.Types
import Potato.Game

instance ToJSON InitialStatePacket where
    toJSON (InitialStatePacket fields units cities timestamp) = object [
              "map" .= fields,
              "cities" .= combinePairs cities,
              "units" .= combinePairs units,
              "timestamp" .= timestamp]
        where
            unionObjects (Object a) (Object b) = a `union` b
            combinePairs :: (ToJSON a, ToJSON b) => [(a,b)] -> [Object]
            combinePairs = map (\(a,b) -> (toJSON a) `unionObjects` (toJSON b))

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