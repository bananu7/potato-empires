{-# LANGUAGE OverloadedStrings #-}
module Main where 
import Web.Scotty
import Data.Aeson.Types

main = scotty 3000 $ do
  get "/" $ do
    json initialMap



data Point = Point Int Int
data MapField = Land | Water deriving (Show, Eq)
type GameMap = [[MapField]]
data City = City String Point
type BattleValue = Int
data Player = Redosia | Bluegaria | Greenland | Shitloadnam deriving (Show, Eq, Ord)
data Unit = Unit BattleValue Player Point
type Timestamp = Int

data InitalStatePacket = InitalStatePacket GameMap [City] [Unit] Timestamp
data UpdatePacket = UpdatePacket [Unit] Timestamp
data MovePacket = MovePacket Point Point

instance ToJSON InitalStatePacket where
	toJSON (InitalStatePacket gameMap cities units timestamp) = object ["map" .= gameMap,
			 "cities" .= cities,
			 "units" .= units,
			 "timestamp" .= timestamp]

instance ToJSON MapField where
	toJSON = toJSON . show

instance ToJSON Player where
	toJSON = toJSON . show

instance ToJSON City where
	toJSON (City name location) = object ["name" .= name, "location" .= location]

instance ToJSON Unit where
	toJSON (Unit value owner location) = object ["value" .= value, "location" .= location, "owner" .= owner]

instance ToJSON Point where
 	toJSON (Point x y) = object ["x" .= x, "y" .= y]

initialMap = replicate 5 $ replicate 5 Land
