{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Potato.Game where 

import Control.Monad.State
import Control.Lens hiding (index)
import Data.Array
import Data.Array.IArray (amap)
import Data.Maybe
import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)
instance Ix Point where
    range (Point minX minY, Point maxX maxY) = [Point x y | x <- [minX .. maxX], y <- [minY .. maxY]] 
    inRange (Point minX minY, Point maxX maxY) (Point x y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY] 
   
    -- implemented the same as default (a,b) Ix instance
    index r@(Point l1 l2, Point u1 u2) p@(Point i1 i2) | inRange r p = index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2 
                                                           | otherwise = error "Out of range"

data FieldType = Land | Water deriving (Show, Eq)

data City = City {
    _name :: String,
    _conqueror :: Maybe Player
} deriving (Show, Eq)

data Unit = Unit {
    _BattleValue :: BattleValue,
    _owner :: Player
} deriving (Show, Eq)

data MapField = MapField { 
    _FieldType :: FieldType,
    _city :: Maybe City,
    _unit :: Maybe Unit
} deriving (Eq)

type GameMap = Array Point MapField

type BattleValue = Int
data Player = Redosia | Shitloadnam deriving (Show, Eq, Ord, Enum)
type Timestamp = Int

data GameState = GameState {
    _GameMap :: GameMap,
    _CurrentPlayer :: Player,
    _timestamp :: Timestamp,
    _currentPlayerMoves :: Int,
    _ActivePlayers :: [Player]
}

data MoveResult = InvalidMove | GameOver | GameContinues deriving (Eq, Show)

createGameState :: GameMap -> GameState
createGameState m = GameState m Redosia 0 defaultPlayerMoves [Redosia, Shitloadnam]
defaultPlayerMoves :: Int
defaultPlayerMoves = 2

makeLenses ''MapField
makeFields ''GameState
makeFields ''City
makeFields ''Unit
makeLenses ''GameState
makeLenses ''City
makeLenses ''Unit

data Move = Move Point Point deriving (Show, Eq)

type GameMonad a = State GameState a

isValid :: Player -> GameState -> Move -> Bool
isValid player game (Move start end) =
    and [isCurrentPlayer, isNotOutOfBounds, isValidUnitAtStart, isValidDestination, isNotTooFar]
    where
        gmap = game ^. gameMap
        
        isCurrentPlayer = player == game ^. currentPlayer

        isNotOutOfBounds = inRange (bounds gmap) start && inRange (bounds gmap) end

        maybeUnitAtStart = (gmap ! start) ^. unit
        isValidUnitAtStart = case maybeUnitAtStart of
            Just unit -> unit ^. owner == game ^. currentPlayer
            Nothing -> False

        isValidDestination = (== Land) $ (gmap ! end) ^. fieldType

        isNotTooFar = (manhattanDistance start end) <= 3
            where manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x2-x1) + abs (y2-y1)
      
-- |Returns Just new game state if the move is valid, nothing otherwise.
--move :: Player -> Move -> GameState -> Maybe GameState
move :: Player -> Move -> GameMonad MoveResult
move p m = get >>= \g -> 
    if (isValid p g m)
    then do
        applyMove p m
        get >>= \g -> if gameOver g then
            return GameOver
        else
            return GameContinues
    else return InvalidMove


applyMove :: Player -> Move -> GameMonad () 
--applyMove :: Player -> Move -> GameState -> GameState
applyMove p (Move start end) = do
    gameMap %= changeDestination
    changeStart
    timestamp %= (+1)
    checkCaptureCity
    generateUnits
    deductPlayerMove
    forceEndTurn
    checkPlayersEndCondition
 where
    checkCaptureCity = (gameMap . ix end . city . traverse . conqueror) .= (Just p)

    changeDestination gm = case maybeOtherUnit of
        Nothing -> setDestinationUnit gm aUnit
        Just otherUnit ->
            if otherUnit ^. owner == p
                then setDestinationUnit gm $ merge aUnit otherUnit
                else setDestinationUnit gm $ battle aUnit otherUnit
        where
            aUnit = fromJust $ (gm ! start) ^. unit
            maybeOtherUnit = (gm ! end) ^. unit
            setDestinationUnit gm u = gm & ix end . unit .~ (Just u)
            merge unitA unitB = unitA & battleValue +~ (unitB ^. battleValue)

    changeStart = (gameMap . ix start . unit .= Nothing)

    generateUnits = get >>= \game -> when (isPlayerLastMove game) $ gameMap . traverse %= generateUnit

    generateUnit field = 
        case getConqueror field of
            Just c -> generateUnit' c field
            Nothing -> field
        where
            getConqueror field = field ^? city . traverse . conqueror . traverse

            generateUnit' :: Player -> MapField -> MapField
            generateUnit' p field =
                case field ^. unit of
                    Just (Unit value p) -> field & unit .~ Just (Unit (value + 5) p)
                    Nothing -> field & unit .~ Just (Unit 5 p)

    deductPlayerMove = get >>= \g -> if isPlayerLastMove g
                          then endTurn
                          else currentPlayerMoves %= subtract 1

    forceEndTurn = get >>= \g -> when (hasNoUnits p g) endTurn
     where
      hasNoUnits p g = isNothing $ g ^? gameMap . traverse . unit . traverse . owner . filtered (== p)

    checkPlayersEndCondition :: GameMonad ()
    checkPlayersEndCondition = get >>= \g -> mapM_ checkPlayerEndCondition (g ^. activePlayers)
    checkPlayerEndCondition p = get >>= \g -> when (hasNoCities p g) $ removePlayer p
     where
        hasNoCities p g = isNothing $ g ^? gameMap . traverse . city . traverse . conqueror . traverse . filtered (== p)

        removePlayer p = do
            activePlayers %= delete p
            (gameMap . traverse . filtered (hasUnitOfPlayer p)) %= (unit `set` Nothing)

        hasUnitOfPlayer p f = (f ^? unit . traverse . owner) == Just p

endTurn :: GameMonad ()
endTurn = do
    get >>= \g -> currentPlayer %= nextPlayer g
    currentPlayerMoves .= defaultPlayerMoves

isPlayerLastMove :: GameState -> Bool 
isPlayerLastMove g = (g ^. currentPlayerMoves) == 1

battle :: Unit -> Unit -> Unit
battle unitA unitB =
    let newUnit = if (unitA ^. battleValue >= unitB ^. battleValue)
                    then unitA & battleValue -~ (unitB ^. battleValue)
                    else unitB & battleValue -~ (unitA ^. battleValue)
    in
       if newUnit ^. battleValue == 0
           then newUnit & (battleValue `set` 1)
           else newUnit          

nextPlayer :: GameState -> Player -> Player
nextPlayer g p = head . tail . dropWhile (/= p) . cycle $ (g ^. activePlayers)

gameOver :: GameState -> Bool
gameOver g = (g ^. currentPlayer) == nextPlayer g (g ^. currentPlayer)
                                
getFieldElemList elem game = elems
    where 
        gmap = game ^. gameMap
        pointsAndFields = (assocs gmap) ^.. traverse.(filtered (\(_, field) -> isJust $ field ^. elem))
        elems = map (\(point, field) -> (point, fromJust $ field ^. elem)) pointsAndFields

getCitiesList :: GameState -> [(Point, City)]
getCitiesList = getFieldElemList city

getUnitsList :: GameState -> [(Point, Unit)]
getUnitsList = getFieldElemList unit

getFieldTypesList :: GameState -> [[FieldType]]
getFieldTypesList game = toListOfLists $ amap (^. fieldType) gmap
    where gmap = game ^. gameMap
          toListOfLists a = [ row y | y <- [minY .. maxY]]
           where
              row y = [ a ! Point x y | x <- [minX .. maxX]]
              (Point minX minY, Point maxX maxY) = bounds a

emptyMap :: GameMap
emptyMap = array mapRange (map (,MapField Land Nothing Nothing) $ range mapRange)
            where
                 mapRange = (Point 0 0, Point 9 9)
