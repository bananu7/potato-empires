{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where 
import Potato.Game
import Potato.Web.App
import StatefulScotty
import Control.Lens hiding (index, (.=))
import Control.Exception (handleJust)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError )
import System.Random
import Control.Monad.State

main = do
    port <- getEnvFallback "PORT" "3000"
    clientDir <- getEnvFallback "POTATO_CLIENT_DIR" "../potato-client"
    generator <- getStdGen
    putStrLn $ "Starting mighty Potato on port " ++ port ++ " with client in '" ++ clientDir ++ "'"

    let (initialRandomMap, generator') = runState randomMap generator
    let initialGameState = createGameState initialRandomMap
    let initialServerState = ServerState { _gameState = initialGameState,
                                           _gen = generator'
                                         }

    startScotty (read port) (app clientDir randomMap) initialServerState
    where
        getEnvFallback :: String -> String -> IO String
        getEnvFallback n f = handleJust extractNotFound (const $ return f) $ getEnv n
        extractNotFound e 
            | isDoesNotExistError e = Just ()
            | otherwise = Nothing

randomMap :: State StdGen GameMap
randomMap = do
    x <- state $ randomR (0, 9)
    y <- state $ randomR (0, 9)
    let pos = Point x y

    return $ emptyMap 
                & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                & (ix (Point 3 4).unit) `set` (Just $ Unit 10 Shitloadnam)
                & (ix (Point 1 1).city) `set` (Just $ City "Cityville" (Just Redosia))
                & (ix (Point 8 8).city) `set` (Just $ City "Townville" (Just Shitloadnam))
                & addRandomCity pos
    where
        addRandomCity pos = (ix pos.city) `set` (Just $ City "Capturetown" Nothing)

initialMap = emptyMap & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                      & (ix (Point 3 4).unit) `set` (Just $ Unit 10 Shitloadnam)
                      & (ix (Point 1 1).city) `set` (Just $ City "Cityville" (Just Redosia))
                      & (ix (Point 8 8).city) `set` (Just $ City "Townville" (Just Shitloadnam))
                      & (ix (Point 4 5).city) `set` (Just $ City "Capturetown" Nothing)
                                
