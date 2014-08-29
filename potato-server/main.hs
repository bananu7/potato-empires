{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where 
import StatefulScotty
import Web.Scotty.Trans
import Data.Aeson.Types hiding (Array)
import Data.Default
import Data.String
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wai.Middleware.RequestLogger
import qualified Control.Monad.State as S
import Control.Lens (makeFields, makeLenses, (+=), (^.))
import Data.Array

data Point = Point Int Int deriving (Show, Eq)
data MapField = Land | Water deriving (Show, Eq)
newtype GameMap = GameMap (Array (Int,Int) MapField)
data City = City {
    _name :: String,
    _cityPosition :: Point
}
type BattleValue = Int
data Player = Redosia | Bluegaria | Greenland | Shitloadnam deriving (Show, Eq, Ord)
data Unit = Unit {
    _battleValue :: BattleValue,
    _owner :: Player,
    _unitPosition :: Point
    } deriving (Show)
type Timestamp = Int

data GameState = GameState {
    _game :: GameMap,
    _cities :: [City],
    _units :: [Unit],
    _timestamp :: Timestamp
    }

makeLenses ''GameState
makeLenses ''City
makeLenses ''Unit

data Move = Move Point Point deriving (Show, Eq)
newtype ValidMove = ValidMove Move

isValid :: Player -> GameState -> Move -> Bool
isValid player game (Move start end) = 
    and [isValidUnitAtStart, isValidDestination]
    where
        --isValidUnitAtStart = not . null . (filter ((== start) . (^. position)) ) $ game ^. units
        isValidUnitAtStart = False
        isValidDestination = False
      

validate :: Player -> GameState -> Move -> Maybe ValidMove
validate player game move = if (isValid player game move) then (Just $ ValidMove move) else Nothing

move :: ValidMove -> GameState -> GameState
move _ g = g

instance Default GameState where
    def = GameState initialMap [] [] 0

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/test" $ do
        t <- webM $ gets _timestamp
        text $ fromString $ show t

    get "/units" $ do
        units <- webM $ S.gets _units
        json units

    get "/addunit" $ do
        --webM $ S.modify $ \ st -> st { _units = (Unit 99 Redosia (Point 0 0)) : _units st }
        redirect "/test"

    get "/" $ do
        json initialMap

    get "/plusone" $ do
        webM $ timestamp += 1
        redirect "/test"

main = startScotty 3000 app

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

initialMap = GameMap $ array ((0,0),(9,9)) [((x,y), Land) | x <- [0..9], y <- [0..9]]

instance ToJSON GameMap where
    toJSON (GameMap a) = toJSON . toListOfLists $ a
        where xSpan = [(fst . fst $ bounds a) .. (fst . snd $ bounds a)]
              ySpan = [(snd . fst $ bounds a) .. (snd . snd $ bounds a)]
              row y = [a ! (x,y) | x <- xSpan ]
              toListOfLists a = [row y | y <- ySpan]
