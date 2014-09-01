{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import Data.Aeson (decode)
import Network.HTTP.Types


setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/cities" $ do
        game <- webM S.get 
        json $ object ["cities" .= (combinePairs $ getCitiesList game)]

    get "/units" $ do 
        game <- webM S.get
        json $ object ["units" .= (combinePairs $ getUnitsList game)]

    get "/map" $ do
        game <- webM S.get
        json $ getFieldTypesList game

    get "/units/add" $ do
        let myNewUnit = (Unit 99 Redosia)
        webM $ gameMap %= (ix (Point 1 1) . unit .~ Just myNewUnit)
        redirect "/units"
    
    get "/" $ do
        game <- webM S.get
        json $ createInitialStatePacket game

    post "/move" $ do
        MovePacket from to <- jsonData
        game <- webM S.get
        let result = move (game ^. currentPlayer) (Move from to) game
        case result of 
            Just newState -> do 
                webM $ S.put newState
                json $ object []
            Nothing -> do
                status $ status400
                text "Invalid move"
