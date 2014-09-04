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

data UpdatePacket = UpdatePacket [(Point, Unit)] [(Point, City)] Player Int Timestamp
createUpdatePacket :: GameState -> UpdatePacket
createUpdatePacket game =
    UpdatePacket
        (getUnitsList game)
        (getCitiesList game)
        (game ^. currentPlayer)
        (game ^. currentPlayerMoves)
        (game ^. timestamp)
