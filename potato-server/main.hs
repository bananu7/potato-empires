{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where 
import Potato.Game
import Potato.Web.App
import StatefulScotty
import Data.Default
import Control.Lens hiding (index, (.=))
import Control.Exception (handleJust)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError )
import System.Random

main = do
    port <- getEnvFallback "PORT" "3000"
    clientDir <- getEnvFallback "POTATO_CLIENT_DIR" "../potato-client"
    generator <- getStdGen
    putStrLn $ "Starting mighty Potato on port " ++ port ++ " with client in '" ++ clientDir ++ "'"
    let initialState = createGameState initialMap generator
    startScotty (read port) (app clientDir initialState) initialState
    where
        getEnvFallback :: String -> String -> IO String
        getEnvFallback n f = handleJust extractNotFound (const $ return f) $ getEnv n
        extractNotFound e 
            | isDoesNotExistError e = Just ()
            | otherwise = Nothing

initialMap = emptyMap & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                      & (ix (Point 3 4).unit) `set` (Just $ Unit 10 Shitloadnam)
                      & (ix (Point 1 1).city) `set` (Just $ City "Cityville" (Just Redosia))
                      & (ix (Point 8 8).city) `set` (Just $ City "Townville" (Just Shitloadnam))
                      & (ix (Point 4 5).city) `set` (Just $ City "Capturetown" Nothing)
                                
