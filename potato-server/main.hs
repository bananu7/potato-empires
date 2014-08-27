{-# LANGUAGE OverloadedStrings #-}
module Main where 
import StatefulScotty
import Web.Scotty.Trans
import Data.Aeson.Types
import Data.Default
import Data.String
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wai.Middleware.RequestLogger

--newtype AppState = AppState { tickCount :: Int }
--instance Default AppState where
--    def = AppState 0 

data GameState = GameState {
    _game :: GameMap,
    _cities :: [City],
    _units :: [Unit],
    _timestamp :: Timestamp
    }
instance Default GameState where
    def = GameState initialMap [] [] 0

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/test" $ do
        t <- webM $ gets _timestamp
        text $ fromString $ show t

    get "/units" $ do
        units <- webM $ gets _units
        json units

    get "/addunit" $ do
        webM $ modify $ \ st -> st { _units = (Unit 99 Redosia (Point 0 0)) : _units st }
        redirect "/test"

    get "/" $ do
        json initialMap

    get "/plusone" $ do
--      webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
        webM $ modify $ \ st -> st { _timestamp = _timestamp st + 1 }
        redirect "/test"

main = startScotty 3000 app

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
