{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

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

data Point = Point Int Int deriving (Show, Eq, Ord)
instance Ix Point where
    range ((Point minX minY), (Point maxX maxY)) = [(Point x y) | x <- [minX .. maxX], y <- [minY .. maxY]] 
    inRange ((Point minX minY), (Point maxX maxY)) (Point x y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY] 
   
    -- implemented the same as default (a,b) Ix instance
    index r@((Point l1 l2), (Point u1 u2)) p@(Point i1 i2) | inRange r p = index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2 
                                                           | otherwise = error "Out of range"                                                           

data MapField = Land | Water deriving (Show, Eq)
newtype GameMap = GameMap { getArray :: Array Point MapField }
data City = City {
    _name :: String,
    _cityPosition :: Point
}
type BattleValue = Int
data Player = Redosia | Bluegaria | Greenland | Shitloadnam deriving (Show, Eq, Ord)
data Unit = Unit {
    _BattleValue :: BattleValue,
    _owner :: Player,
    _unitPosition :: Point
    } deriving (Show)
type Timestamp = Int

data GameState = GameState {
    _GameMap :: GameMap,
    _cities :: [City],
    _units :: [Unit],
    _timestamp :: Timestamp
    }

makeFields ''GameState
makeFields ''City
makeFields ''Unit
makeLenses ''GameState
makeLenses ''City
makeLenses ''Unit

data Move = Move Point Point deriving (Show, Eq)

isValid :: Player -> GameState -> Move -> Bool
isValid player game (Move start end) =
    and [isNotOutOfBounds, isValidUnitAtStart, isValidDestination, isNotTooFar]
    where
        gmap = getArray $ game ^. gameMap
        
        isNotOutOfBounds = inRange (bounds gmap) start && inRange (bounds gmap) end

        isValidUnitAtStart = not . null . (filter ((== start) . (^. position)) ) $ game ^. units

        isValidDestination = (gmap ! end) == Land

        isNotTooFar = (manhattanDistance start end) <= 3
            where manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x2-x1) + abs (y2-y1)
      
-- |Returns Just new game state if the move is valid, nothing otherwise.
move :: Player -> Move -> GameState -> Maybe GameState
move p m g = if (isValid p g m) then Just g else Nothing

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

initialMap = GameMap $ array mapRange (map (,Land) $ range mapRange)
             where
                 mapRange = ((Point 0 0), (Point 9 9))
                
instance ToJSON GameMap where
    toJSON (GameMap a) = toJSON . toListOfLists $ a
        where ((Point minX minY), (Point maxX maxY)) = bounds a
              row y = [ a ! (Point x y) | x <- [minX .. maxX]]
              toListOfLists a = [ row y | y <- [minY .. maxY]]
