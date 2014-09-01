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
import Web.Scotty.Trans hiding (get)
import qualified Web.Scotty.Trans as Scotty (get)
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
import Data.HashMap.Strict (union)

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

get r a = Scotty.get r $ do
    setCorsHeader
    a

app :: ScottyT Text (WebM GameState) ()
app = do
    middleware logStdoutDev

    get "/test" $ do
        t <- webM $ gets _timestamp
        text $ fromString $ show t

    get "/cities" $ do
        game <- webM S.get 
        json $ getCitiesList game

    get "/units" $ do 
        game <- webM S.get
        json $ getUnitsList game

    get "/map" $ do
        game <- webM S.get
        json $ getFieldTypesList game

    get "/addunit" $ do
        let myNewUnit = (Unit 99 Redosia)
        webM $ gameMap %= (ix (Point 1 1) . unit .~ Just myNewUnit)
        redirect "/units"
    
    get "/" $ do
        game <- webM S.get
        json $ createInitialStatePacket game

    get "/plusone" $ do
        webM $ timestamp += 1
        redirect "/test"
