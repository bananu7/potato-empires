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
import Data.Array.IArray (amap)
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

getFieldElemList elem game = elems
    where 
        gmap = game ^. gameMap
        pointsAndFields = (assocs gmap) ^.. traversed.(filtered (\(pos, field) -> isJust $ field ^. elem))
        elems = map (\(point, field) -> (point, fromJust $ field ^. elem)) pointsAndFields

getCitiesList = getFieldElemList city
getUnitsList = getFieldElemList unit

getFieldTypesList game = toListOfLists $ amap (view fieldType) gmap
    where gmap = game ^. gameMap
          toListOfLists a = [ row y | y <- [minY .. maxY]]
           where
              row y = [ a ! (Point x y) | x <- [minX .. maxX]]
              ((Point minX minY), (Point maxX maxY)) = bounds a

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/test" $ do
        t <- webM $ gets _timestamp
        text $ fromString $ show t

    get "/cities" $ do
        game <- webM S.get 
        json $ getCitiesList game

    get "/units" $ do 
        game <- webM S.get
        json $ getUnitsList game

    get "/map" $ do
        game <- webM S.get
        json $ getFieldTypesList game

    get "/addunit" $ do
        --webM $ S.modify $ \ st -> st { _units = (Unit 99 Redosia (Point 0 0)) : _units st }
        let myNewUnit = (Unit 99 Redosia)
        webM $ gameMap %= (ix (Point 1 1) . unit .~ Just myNewUnit)
        redirect "/units"
    
    get "/" $ do
        game <- webM S.get
        json $ createInitialStatePacket game

    get "/plusone" $ do
        webM $ timestamp += 1
        redirect "/test"

main = startScotty 3000 app



--data InitalStatePacket = InitalStatePacket GameMap Timestamp
data InitialStatePacket = InitialStatePacket [[FieldType]] [(Point, Unit)] [(Point, City)] Timestamp
createInitialStatePacket :: GameState -> InitialStatePacket
createInitialStatePacket game =
    InitialStatePacket 
        (getFieldTypesList game)
        (getUnitsList game)
        (getCitiesList game)
        (game ^. timestamp)

instance ToJSON InitialStatePacket where
    toJSON (InitialStatePacket fields units cities timestamp) = object [
              "map" .= fields,
              "cities" .= cities,
              "units" .= units,
              "timestamp" .= timestamp]

--data UpdatePacket = UpdatePacket [Unit] Timestamp
-- data MovePacket = MovePacket Point Point
-- 

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
    toJSON = toJSON . show

instance ToJSON Player where
    toJSON = toJSON . show

instance ToJSON City where
    toJSON (City name) = object ["name" .= name]

instance ToJSON Unit where
    toJSON (Unit value owner) = object ["value" .= value, "owner" .= owner]

instance ToJSON Point where
    toJSON (Point x y) = object ["x" .= x, "y" .= y]

initialMap = emptyMap & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                & (ix (Point 2 2).city) `set` (Just $ City "Cityville")
             where
                 emptyMap = array mapRange (map (,MapField Land Nothing Nothing) $ range mapRange)
                 mapRange = ((Point 0 0), (Point 5 5))
               
