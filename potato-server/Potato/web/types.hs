{-# LANGUAGE DeriveGeneric #-}
module Potato.Web.Types where
import Potato.Game
import Control.Lens
import GHC.Generics

data InitialStatePacket = InitialStatePacket [[FieldType]] [(Point, Unit)] [(Point, City)] Timestamp
createInitialStatePacket :: GameState -> InitialStatePacket
createInitialStatePacket game =
    InitialStatePacket 
        (getFieldTypesList game)
        (getUnitsList game)
        (getCitiesList game)
        (game ^. timestamp)

data MovePacket = MovePacket Point Point deriving (Show, Generic)