{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Potato.Web.App where 
import Prelude hiding (lookup)
import Potato.Game
import Potato.Types
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
import System.Random
import Control.Applicative
import Data.HashMap.Lazy
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Text.Read (readMaybe)
import Data.Text.Lazy (unpack)

type PlayerTokenMap = HashMap Token Player 

data ServerState = ServerState {
    _gameState :: GameState,
    _tokens :: PlayerTokenMap,
    _gen :: StdGen
    }

makeLenses ''ServerState


createPlayerTokens :: PlayerTokenMap    
createPlayerTokens = fromList [(1, Redosia), (2, Shitloadnam)]

-- Those helpers make writing handlers below a bit more convenient

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get

safeParam name = (Just <$> param name) `rescue` (const $ return Nothing)

emptyJsonResponse = json $ object []

-- This is a rather generic function that allows easily
-- narrowing a State computation via a Lens.
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

getGameState = (view gameState) <$> getWebMState
runGameState x = runWebMState $ hoistStateWithLens gameState x
runRandom x = runWebMState $ hoistStateWithLens gen x

data MoveHandlerError = NoAuthHeader | MalformedHeader | NoSuchPlayer deriving (Eq, Show)

app :: String -> S.State StdGen GameMap -> ScottyT Text (WebM ServerState) ()
app clientDir mapGenerator = do
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

    -- sample use of runRandom
    --get "/random" $ do
    --    x <- runRandom $ state . randomR (1 :: Int,100)
    --    text . pack . show $ x

    post "/move" $ do
        ts <- runWebMState $ use tokens

        eitherPlayerError <- runEitherT $ extractHeader >>= parseHeader >>= findPlayer ts       

        case eitherPlayerError of
            (Right player) -> do
                MovePacket from to <- jsonData
                moveResult <- runGameState $ do
                    moveResult <- move player (Move from to)
                    return moveResult

                case moveResult of
                    GameOver -> do
                        newGS <- createGameState <$> runRandom mapGenerator
                        runGameState $ S.put newGS
                        emptyJsonResponse
                    GameContinues -> emptyJsonResponse
                    InvalidMove -> do
                        status status400
                        text "Invalid move"

            (Left NoAuthHeader) -> do
                setHeader "WWW-Authenticate" "Token realm=\"/\""
                status status401

            (Left MalformedHeader) -> do
                status status401

            (Left NoSuchPlayer) -> do
                status status401           

    where
        extractHeader = do
            maybeTokenText <- lift (header "Authorization")
            (unpack <$> maybeTokenText) `valueOr` NoAuthHeader

        parseHeader tokenString = parseToken tokenString `valueOr` MalformedHeader
        findPlayer tokens token = lookup token tokens `valueOr` NoSuchPlayer

        valueOr mval err = case mval of
            Just val -> right val
            Nothing -> left err

        parseToken str = p (words str)
            where p :: [String] -> Maybe Token
                  p ["Token", token] = readMaybe token
                  p _ = Nothing
