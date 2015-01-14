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
import Control.Monad.Trans.Either
import Control.Monad.Trans (lift)
import Text.Read (readMaybe)
import Data.Text.Lazy (unpack, pack)
import Data.Maybe
import qualified Data.List (find)
import Data.Map
import Data.Map.Lens

type PlayerTokenMap = [(Token,Player)]

lookupPlayer :: Player -> PlayerTokenMap -> Maybe Token 
lookupPlayer player xs = fst <$> Data.List.find ((== player) . snd) xs

lookupToken :: Token -> PlayerTokenMap -> Maybe Player 
lookupToken token xs = snd <$> Data.List.find ((== token) . fst) xs

data TableState = TableState {
    _tokens :: PlayerTokenMap,
    _desiredPlayerCount :: Int
    }
makeLenses ''TableState

isTableFull :: TableState -> Bool
isTableFull t = (length $ t ^. tokens) == (t ^. desiredPlayerCount)

type TableId = Int
data ServerState = ServerState {
    _gameState :: GameState,
    _tables :: Map TableId TableState,
    _gen :: StdGen
    }
makeLenses ''ServerState

instance Parsable Player where
    parseParam = readEither

createTable = TableState [] 2

-- Those helpers make writing handlers below a bit more convenient

setCorsHeader = setHeader "Access-Control-Allow-Origin" "*"

executeWithCors method r action = method r $ do
    setCorsHeader
    action

post = executeWithCors Scotty.post
get = executeWithCors Scotty.get
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
    
    get "/initial" $ do
        game <- getGameState
        json $ createInitialStatePacket game

    get "/" $ do
        file $ clientDir ++ "/index.html"

    get "/update" $ do
        game <- getGameState
        json $ createUpdatePacket game

    post "/request-token" handlerRequestToken
    post "/move" $ handlerMove mapGenerator
 
handlerRequestToken = do 
    player <- param "player"
    tableId <- param "table"
    maybeResult <- runWebMState $ do        
        maybeTable <- lookup tableId <$> use tables 
        let maybeTokens = (view tokens) <$> maybeTable
        let maybePlayer = lookupPlayer player <$> maybeTokens

        if isJust maybePlayer
            then return Nothing -- seat taken
            else do
                -- This use of `fromJust` is okay because of the informal invariant
                -- if maybePlayer lookup succeeded, maybeTokens must also be ok
                token <- hoistStateWithLens gen $ randomWithRepick lookupToken (fromJust maybeTokens) (1,1000)
                tables . at tableId . traverse . tokens %= ((token, player) :)
                return $ Just token
                
    case maybeResult of
        Just token -> text . pack $ show token
        Nothing -> do
            status status403
            text "Token request failed"
 where
    randomWithRepick fLookup xs range = do
        x <- S.state $ randomR range
        if isNothing $ fLookup x xs
            then return x
            else randomWithRepick fLookup xs range

data MoveHandlerError = NoAuthHeader | MalformedHeader | NoSuchPlayer | NoSuchGame deriving (Eq, Show)
handlerMove mapGenerator = do
    tableId <- param "table"

    eitherPlayerError <- lift $ runEitherT $ findPlayer <$> (getTokenList tableId) <*> (extractHeader >>= parseHeader)

    case eitherPlayerError of
        Right player -> do
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

        -- Additional information about the reason of failure resides
        -- in the ignored Left field; not used right now.
        Left _ -> do
            setHeader "WWW-Authenticate" "Token realm=\"/\""
            status status401

  where
    getTokenList tableId = runWebMState $ do
        tableMap <- use tables
        let maybeTable = lookup tableId tableMap
        let maybeTokens = (view tokens) <$> maybeTable
        return $ maybeTokens `valueOr` NoSuchGame

    extractHeader = do
        maybeTokenText <- lift (header "Authorization")
        (unpack <$> maybeTokenText) `valueOr` NoAuthHeader

    parseHeader tokenString = parseToken tokenString `valueOr` MalformedHeader
    findPlayer tokens token = lookupToken token tokens `valueOr` NoSuchPlayer

    valueOr mval err = case mval of
        Just val -> right val
        Nothing -> left err

    parseToken str = p (words str)
        where p :: [String] -> Maybe Token
              p ["Token", token] = readMaybe token
              p _ = Nothing
