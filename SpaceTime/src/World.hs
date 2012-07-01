module World where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array
import Data.Map (Map)
import qualified Data.Map as Map
import Control.DeepSeq

import Configuration
import Unit
import Widget

data World = World 
	{	units          :: Map Int Unit
	,	terrain        :: Array Int (Array Int Bool)
	,	slice          :: Int
	,	time           :: Float
	,	c_history      :: [Slice]
	,	evolve_mode    :: Evolving
	,	last_tick      :: Float
	,	mode           :: Mode
	,	selected       :: [UnitID]
	,	mouse_l_down   :: Bool
	,	mouse_coord    :: (Float, Float)
	,	world_widgets  :: [Widget World]
	,	current_widget :: Maybe (Widget World)
	}

empty_world :: World
empty_world = World
	{	units          = Map.empty
	,	terrain        = array (1,1) [(1,array (1,1) [(1,False)])]
	,	slice          = 0
	,	time           = 0
	,	c_history      = []
	,	evolve_mode    = Static
	,	last_tick      = 0.0
	,	mode           = ModeSelect
	,	selected       = [1,2]
	,	mouse_l_down   = False
	,	mouse_coord    = (0, 0)
	,	current_widget = Nothing
	,	world_widgets  = []
	}

instance WidgetState World where
	provide_widgets    = world_widgets
	get_current_widget = current_widget
	set_current_widget widget world = world {current_widget = widget}

data Evolving = Evolving | Devolving | Static deriving (Eq)

tick_forward :: World -> World
tick_forward world @ World {slice = i} = world {slice = mod (i+1) slice_max}

tick_back :: World -> World
tick_back world @ World {slice = i} = world {slice = mod (i-1) slice_max}

main_time_forward :: Float -> World -> World
main_time_forward f (world @ World {time = t}) = world {time = t+f}

init_grid :: World -> Grid
init_grid world = map snd (Map.toList (units world))

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

