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
import Network.Wai.Middleware.Static

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get

--hoistState :: Monad m => S.State s a -> S.StateT s m a
--hoistState = S.StateT . (return .) . S.runState

hoistState :: (Monad m, S.MonadState s m) => S.State s a -> m a
hoistState op = do
    x <- S.get
    let (r, x') = S.runState op x
    S.put x'
    return r

app :: String -> GameState -> ScottyT Text (WebM GameState) ()
app clientDir defaultGameState = do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase clientDir)

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
    
    get "/initial" $ do
        game <- webM S.get
        json $ createInitialStatePacket game

    get "/" $ do
        file $ clientDir ++ "/index.html"

    get "/update" $ do
        game <- webM S.get
        json $ createUpdatePacket game

    post "/move" $ do
        MovePacket from to <- jsonData
        moveResult <- webM $ hoistState $ do
            currentPlayer <- fmap (view currentPlayer) S.get
            move currentPlayer (Move from to)
                
        case moveResult of
            GameOver -> do
                webM $ S.put defaultGameState
                json $ object []
            GameContinues -> json $ object []
            InvalidMove -> do
                status status400
                text "Invalid move"
