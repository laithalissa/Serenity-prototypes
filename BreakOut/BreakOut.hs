
import Control.Wire.Wire
import GHC.Float
import Graphics.Gloss.Interface.Pure.Game

import Game
import Models
import Keyboard

main = play
	(InWindow "FUCKING GLOSS" (600, 400) (100, 100))
	white
	20
	initWorld
	drawWorld
	eventCallback
	stepCallback

data World = World { worldKeyboard :: Keyboard, worldWire :: GameWire, worldPics :: [Picture] }

initWorld = World {
	worldKeyboard = initKeyboard
,	worldWire = game
,	worldPics = []
}

drawWorld :: World -> Picture
drawWorld world = Pictures (worldPics world)

eventCallback :: Event -> World -> World
eventCallback (EventKey key state _ _) world = world { worldKeyboard = handleKeyEvent key state (worldKeyboard world) }
eventCallback _ world = world

stepCallback :: Float -> World -> World
stepCallback delta world@(World kb game _) = newWorld
	where
	newWorld = case result of
		Left _ -> world { worldWire = game' }
		Right p -> world { worldWire = game', worldPics = p }
	(result, game') = stepWireP game 1.0 kb
