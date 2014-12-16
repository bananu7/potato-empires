{-# LANGUAGE TemplateHaskell #-}

module Potato.Types where

import Control.Monad.State
import System.Random
import Data.Ix
import Data.Array
import Control.Lens hiding (index)

type RandomM a = State StdGen a

data Point = Point Int Int deriving (Show, Eq, Ord)
instance Ix Point where
    range (Point minX minY, Point maxX maxY) = [Point x y | x <- [minX .. maxX], y <- [minY .. maxY]] 
    inRange (Point minX minY, Point maxX maxY) (Point x y) = and [x >= minX, y >= minY, x <= maxX, y <= maxY] 
   
    -- implemented the same as default (a,b) Ix instance
    index r@(Point l1 l2, Point u1 u2) p@(Point i1 i2) | inRange r p = index (l1,u1) i1 * rangeSize (l2,u2) + index (l2,u2) i2 
                                                       | otherwise = error "Out of range"

data Player = Redosia | Shitloadnam deriving (Show, Read, Eq, Ord, Enum)

data City = City {
    _name :: String,
    _conqueror :: Maybe Player
} deriving (Show, Eq)

makeFields ''City
makeLenses ''City

type BattleValue = Int

data Unit = Unit {
    _BattleValue :: BattleValue,
    _owner :: Player
} deriving (Show, Eq)

makeFields ''Unit
makeLenses ''Unit

data FieldType = Land | Water deriving (Show, Eq)

data MapField = MapField { 
    _FieldType :: FieldType,
    _city :: Maybe City,
    _unit :: Maybe Unit
} deriving (Eq)

makeFields ''MapField
makeLenses ''MapField

type Timestamp = Int

type GameMap = Array Point MapField

type Token = Int
