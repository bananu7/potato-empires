{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ReaderT to provide access
-- to a TVar containing global state.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module StatefulScotty(
      WebM(..)
    , webM
    , gets
    , modify
    , startScotty
) where

import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Default
--import Data.String
--import Data.Text.Lazy (Text)

import Web.Scotty.Trans
import Control.Monad.State.Class

-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- effectively meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Also note: your monad must be an instance of 'MonadIO' for
-- Scotty to use it.
newtype WebM appState a = WebM { runWebM :: ReaderT (TVar appState) IO a }
    deriving (Monad, MonadIO, MonadReader (TVar appState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM appState a -> t (WebM appState) a
webM = lift

instance MonadState s (WebM s) where
    get = ask >>= liftIO . readTVarIO >>= return
    put x = ask >>= liftIO . atomically . flip writeTVar x

-- Some helpers to make this feel more like a state monad.
--gets :: (appState -> b) -> WebM appState b
--gets f = ask >>= liftIO . readTVarIO >>= return . f

--modify :: (appState -> appState) -> WebM appState ()
--modify f = ask >>= liftIO . atomically . flip modifyTVar' f

startScotty port app = do 
    sync <- newTVarIO def  
    let runM m = runReaderT (runWebM m) sync
        runActionToIO = runM
    scottyT port runM runActionToIO app

{-
main :: IO ()
main = do
    sync <- newTVarIO def
        -- Note that 'runM' is only called once, at startup.
    let runM m = runReaderT (runWebM m) sync
        -- 'runActionToIO' is called once per action.
        runActionToIO = runM

    scottyT 3000 runM runActionToIO app

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT Text WebM ()
app = do
    middleware logStdoutDev
    get "/" $ do
        c <- webM $ gets tickCount
        text $ fromString $ show c

    get "/plusone" $ do
        webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
        redirect "/"

    get "/plustwo" $ do
        webM $ modify $ \ st -> st { tickCount = tickCount st + 2 }
        redirect "/"

-}
