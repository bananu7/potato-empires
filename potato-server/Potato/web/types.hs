module Potato.Web.Types where
import Potato.Game
import Control.Lens

data InitialStatePacket = InitialStatePacket [[FieldType]] [(Point, Unit)] [(Point, City)] Timestamp
createInitialStatePacket :: GameState -> InitialStatePacket
createInitialStatePacket game =
    InitialStatePacket 
        (getFieldTypesList game)
        (getUnitsList game)
        (getCitiesList game)
        (game ^. timestamp)