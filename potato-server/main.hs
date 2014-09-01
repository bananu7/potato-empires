{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where 
import Potato.Game
import Potato.Web.App
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

main = startScotty 3000 app

initialMap = emptyMap & (ix (Point 0 1).unit) `set` (Just $ Unit 12 Redosia)
                      & (ix (Point 2 2).city) `set` (Just $ City "Cityville")
                      & (ix (Point 3 4).unit) `set` (Just $ Unit 10 Shitloadnam)
             where
                 emptyMap = array mapRange (map (,MapField Land Nothing Nothing) $ range mapRange)
                 mapRange = ((Point 0 0), (Point 9 9))
               
instance Default GameState where
  def = GameState initialMap Redosia 0