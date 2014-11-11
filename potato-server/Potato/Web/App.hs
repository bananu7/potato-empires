{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Potato.Web.App where 
import Potato.Game
import Potato.Web.Serialization
import Potato.Web.Types
import StatefulScotty
import Web.Scotty.Trans hiding (get, post)
import qualified Web.Scotty.Trans as Scotty (get, post)
import Data.Aeson.Types hiding (Array)
import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger
import qualified Control.Monad.State as S
import Control.Lens hiding (index, (.=))
import Network.HTTP.Types
import Network.Wai.Middleware.Static

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get

emptyJsonResponse = json $ object []

app :: String -> GameState -> ScottyT Text (WebM GameState) ()
app clientDir defaultGameState = do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase clientDir)

    get "/cities" $ do
        game <- getWebMState
        json $ object ["cities" .= (combinePairs $ getCitiesList game)]

    get "/units" $ do 
        game <- getWebMState
        json $ object ["units" .= (combinePairs $ getUnitsList game)]

    get "/map" $ do
        game <- getWebMState
        json $ getFieldTypesList game

    get "/units/add" $ do
        let myNewUnit = (Unit 99 Redosia)
        runWebMState $ gameMap %= (ix (Point 1 1) . unit .~ Just myNewUnit)
        redirect "/units"
    
    get "/initial" $ do
        game <- getWebMState
        json $ createInitialStatePacket game

    get "/" $ do
        file $ clientDir ++ "/index.html"

    get "/update" $ do
        game <- getWebMState
        json $ createUpdatePacket game

    post "/move" $ do
        MovePacket from to <- jsonData
        moveResult <- runWebMState $ do
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
