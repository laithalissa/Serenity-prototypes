module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Control.DeepSeq

import Configuration
import Unit

data World = World 
	{	units          :: Map Int Unit
	,	terrain        :: Array Int (Array Int Bool)
	,	slice          :: Int
	,	time           :: Float
	,	c_history      :: [Slice]
	,	evolving       :: Bool
	,	last_tick      :: Float
	,	mode           :: Mode
	,	selected       :: [UnitID]
	,	mouse_l_down   :: Bool
	,	mouse_coord    :: (Float, Float)
	,	world_widgets  :: [Widget]
	,	current_widget :: Maybe Widget
	}

empty_world :: World
empty_world = World
	{	units          = Map.empty
	,	terrain        = array (1,1) [(1,array (1,1) [(1,False)])]
	,	slice          = 0
	,	time           = 0
	,	c_history      = []
	,	evolving       = False
	,	last_tick      = 0.0
	,	mode           = ModeSelect
	,	selected       = [1,2]
	,	mouse_l_down   = False
	,	mouse_coord    = (0, 0)
	,	current_widget = Nothing
	,	world_widgets  = []
	}

init_grid :: World -> Grid
init_grid world = map snd (Map.toList (units world))

tick_forward :: World -> World
tick_forward world @ World {slice = i} = world {slice = mod (i+1) sliceMax}

tick_back :: World -> World
tick_back world @ World {slice = i} = world {slice = i-1`mod` sliceMax}

main_time_forward :: Float -> World -> World
main_time_forward f (world @ World {time = t}) = world {time = t+f}

type Grid = [Unit]
type Time = Int
type Terrain = Array Int (Array Int Bool)
type Grid' = Map (Int, Int) [Unit]

data Slice = Slice Grid Time

instance NFData Slice where
	rnf (Slice grid time) = seq (deepseq grid time) ()

instance NFData Unit where
	rnf unit = seq (planned_path unit) ()

data Mode = ModeSelect | ModeMove | ModeAttack | ModeAttackMove deriving Eq

data Widget = Widget -- (Float, Float) (Float, Float) (World -> IO World) (World -> IO World) (World -> IO World)
	{	bottom_left     :: (Float, Float)
	,	top_right       :: (Float, Float)
	,	mouse_up_cb     :: Event -> World -> IO World
	,	mouse_down_cb   :: Event -> World -> IO World
	,	mouse_motion_cb :: Event -> World -> IO World
	,	widget_picture  :: World -> Picture
	}

nothing_widget :: Widget
nothing_widget = Widget
	{	bottom_left     = (0,0)
	,	top_right       = (0,0)
	,	mouse_up_cb     = \_ world -> return world
	,	mouse_down_cb   = \_ world -> return world
	,	mouse_motion_cb = \_ world -> return world
	,	widget_picture  = \_       -> Pictures []
	}

