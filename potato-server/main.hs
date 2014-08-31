{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Lens hiding (index, (.=))
import Data.Array
import Data.Maybe

data Point = Point Int Int deriving (Show, Eq, Ord)
instance Ix Point where
    range ((Point minX minY), (Point maxX maxY)) = [(Point x y) | x <- [minX .. maxX], y <- [minY .. maxY]] 
    inRange ((Point minX minY), (Point maxX maxY)) (Point x y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY] 
   
    -- implemented the same as default (a,b) Ix instance
    index r@((Point l1 l2), (Point u1 u2)) p@(Point i1 i2) | inRange r p = index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2 
                                                           | otherwise = error "Out of range"                                                           
data FieldType = Land | Water deriving (Show, Eq)
data MapField = MapField { 
    _FieldType :: FieldType,
    _city :: Maybe City,
    _unit :: Maybe Unit
}

type GameMap = Array Point MapField

data City = City {
    _name :: String
} deriving (Show)
type BattleValue = Int
data Player = Redosia | Bluegaria | Greenland | Shitloadnam deriving (Show, Eq, Ord)
data Unit = Unit {
    _BattleValue :: BattleValue,
    _owner :: Player
    } deriving (Show)
type Timestamp = Int

data GameState = GameState {
    _GameMap :: GameMap,
    _timestamp :: Timestamp
    }

makeLenses ''MapField
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
        gmap = game ^. gameMap
        
        isNotOutOfBounds = inRange (bounds gmap) start && inRange (bounds gmap) end

        isValidUnitAtStart = isJust $ (gmap ! start) ^. unit

        isValidDestination = (isNothing $ (gmap ! end) ^. unit) &&
                             ((== Land) $ (gmap ! end) ^. fieldType)

        isNotTooFar = (manhattanDistance start end) <= 3
            where manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x2-x1) + abs (y2-y1)
      
-- |Returns Just new game state if the move is valid, nothing otherwise.
move :: Player -> Move -> GameState -> Maybe GameState
move p m@(Move start end) g = if (isValid p g m) then Just applyMove else Nothing
                               where
                                aUnit = (g ^. gameMap) ! start ^. unit
                                
                                changeMap = (ix end . unit .~ aUnit) . 
                                            (ix start . unit .~ Nothing)
                               
                                applyMove = g & gameMap %~ changeMap
                                
instance Default GameState where
    def = GameState initialMap 0

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/test" $ do
        t <- webM $ gets _timestamp
        text $ fromString $ show t

--    get "/units" $ do
        --units <- webM $ S.gets >>= 
        --json units

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

instance Show MapField where
    show (MapField f u c) = fs ++ us ++ cs
        where fs = show f
              us = if isJust u then show (fromJust u)
                               else ""
              cs = if isJust c then show (fromJust c)
                               else ""

instance ToJSON MapField where
    toJSON = toJSON . show

instance ToJSON Player where
    toJSON = toJSON . show

instance ToJSON City where
    toJSON (City name) = object ["name" .= name]

instance ToJSON Unit where
    toJSON (Unit value owner) = object ["value" .= value, "owner" .= owner]

instance ToJSON Point where
    toJSON (Point x y) = object ["x" .= x, "y" .= y]

initialMap = array mapRange (map (,MapField Land Nothing Nothing) $ range mapRange)
             where
                 mapRange = ((Point 0 0), (Point 9 9))
                
instance ToJSON GameMap where
    toJSON a = toJSON . toListOfLists $ a
        where ((Point minX minY), (Point maxX maxY)) = bounds a
              row y = [ a ! (Point x y) | x <- [minX .. maxX]]
              toListOfLists a = [ row y | y <- [minY .. maxY]]
