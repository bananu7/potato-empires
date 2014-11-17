{-# LANGUAGE TupleSections #-}

module Potato.GameMap (
    GameMap,
    randomMap,
    emptyMap
) where

import Data.Array
import Potato.Types
import Control.Lens
import Data.Maybe
import Control.Monad.State
import System.Random

-- |Generates random map using RandomM monad.
randomMap :: RandomM GameMap
randomMap = do
    let map = emptyMap
                & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                & (ix (Point 3 4).unit) `set` (Just $ Unit 10 Shitloadnam)
                & (ix (Point 1 1).city) `set` (Just $ City "Cityville" (Just Redosia))
                & (ix (Point 8 8).city) `set` (Just $ City "Townville" (Just Shitloadnam))
    
    map' <- changeRandomTilesToWater 10 map
    map'' <- addRandomCities 6 map'
    return map''

-- Helpers start here

emptyMap :: GameMap
emptyMap = array mapRange (map (,MapField Land Nothing Nothing) $ range mapRange)
            where
                 mapRange = (Point 0 0, Point 9 9)


actNTimes op n m = foldl (>>=) (return m) (replicate n op)

addRandomCities = actNTimes addRandomCity

addRandomCity m = do
    pos <- generateRandomPointWith validCityPlace m
    return $ m & (ix pos.city) `set` (Just $ City "Capturetown" Nothing)
    where
        validCityPlace m p = (isNothing $ m ! p ^. unit) &&
                             (isNothing $ m ! p ^. city) &&
                             ((== Land) $ m ! p ^. fieldType)

changeRandomTilesToWater = actNTimes changeRandomTileToWater

changeRandomTileToWater :: GameMap -> RandomM GameMap
changeRandomTileToWater m = do
    pos <- generateRandomPointWith valid m
    return $ m & (ix pos . fieldType) `set` Water
    where
        valid m p = (isNothing $ m ! p ^. unit) &&
                    (isNothing $ m ! p ^. city) &&
                    ((== Land) $ m ! p ^. fieldType)

generateRandomPointWith pred m = do
    pos <- generateRandomPoint
    if (pred m pos) then return pos
                    else generateRandomPointWith pred m

generateRandomPoint :: RandomM Point
generateRandomPoint = do
    x <- state $ randomR (0, 9)
    y <- state $ randomR (0, 9)
    return $ Point x y
