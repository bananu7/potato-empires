{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where 
import Potato.Game
import Potato.Web.App
import Potato.GameMap
import StatefulScotty
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
                                           _gen = generator',
                                           _tableState = createTable
                                         }

    startScotty (read port) (app clientDir randomMap) initialServerState
    where
        getEnvFallback :: String -> String -> IO String
        getEnvFallback n f = handleJust extractNotFound (const $ return f) $ getEnv n
        extractNotFound e 
            | isDoesNotExistError e = Just ()
            | otherwise = Nothing
