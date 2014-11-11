{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Potato.Web.App where 
import Potato.Game
import Potato.Web.Serialization
import Potato.Web.Types
import StatefulScotty
import Web.Scotty.Trans hiding (get, post)
import qualified Web.Scotty.Trans as Scotty (get, post)
import Data.Aeson.Types hiding (Array)
import Data.Text.Lazy (Text, pack)
import Network.Wai.Middleware.RequestLogger
import qualified Control.Monad.State as S
import Control.Lens hiding (index, (.=))
import Network.HTTP.Types
import Network.Wai.Middleware.Static
import System.Random

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get

emptyJsonResponse = json $ object []

data ServerState = ServerState {
    _gameState :: GameState,
    _gen :: StdGen
    }

makeLenses ''ServerState

--hoistStateWithLens :: Simple Lens ServerState GameState -> S.State GameState a -> S.State ServerState a
hoistStateWithLens :: S.MonadState outerState m => 
                      Simple Lens outerState innerState -> 
                      S.State innerState a ->
                      m a
hoistStateWithLens acc op = do
    s <- S.get
    let sp = s ^. acc
    let (res, sp') = S.runState op sp
    S.put (s & acc .~ sp')
    return res

getGameState = fmap (view gameState) getWebMState
runGameState x = runWebMState $ hoistStateWithLens gameState x
runRandom x = runWebMState $ hoistStateWithLens gen (S.state x)
        
app :: String -> GameState -> ScottyT Text (WebM ServerState) ()
app clientDir defaultGameState = do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase clientDir)

    get "/cities" $ do
        game <- getGameState
        json $ object ["cities" .= (combinePairs $ getCitiesList game)]

    get "/units" $ do 
        game <- getGameState
        json $ object ["units" .= (combinePairs $ getUnitsList game)]

    get "/map" $ do
        game <- getGameState
        json $ getFieldTypesList game

    get "/units/add" $ do
        let myNewUnit = (Unit 99 Redosia)
        runGameState $ gameMap %= (ix (Point 1 1) . unit .~ Just myNewUnit)
        redirect "/units"
    
    get "/initial" $ do
        game <- getGameState
        json $ createInitialStatePacket game

    get "/" $ do
        file $ clientDir ++ "/index.html"

    get "/update" $ do
        game <- getGameState
        json $ createUpdatePacket game

    get "/random" $ do
        x <- runRandom $ randomR (1 :: Int,100)
        text . pack . show $ x

    post "/move" $ do
        MovePacket from to <- jsonData
        moveResult <- runGameState $ do
            currentPlayer <- fmap (view currentPlayer) S.get
            moveResult <- move currentPlayer (Move from to)
            S.when (moveResult == GameOver) $ S.put defaultGameState
            return moveResult

        case moveResult of
            GameOver -> emptyJsonResponse
            GameContinues -> emptyJsonResponse
            InvalidMove -> do
                status status400
                text "Invalid move"
