module Main (
	main
) 
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.DeepSeq
import qualified Data.Map as Map

import SpaceTime.World
import SpaceTime.Unit
import SpaceTime.History
import SpaceTime.Configuration
import SpaceTime.Graphics
import SpaceTime.Terrain

initial_units = Map.fromList
	[	(1, basic_unit 1 1 (10,50))
	,	(2, basic_unit 2 1 (15,50))
	,	(3, basic_unit 3 1 (30,50))
	,	(4, basic_unit 4 2 (20,10))
	,	(5, basic_unit 5 2 (35,10))
	,	(6, basic_unit 6 2 (45,10))
	]

main :: IO ()
main = do
	let start1 = empty_world
		{	units = initial_units
		,	terrain = initial_terrain
		}
	let start = start1 
		{	c_history = force $ history start1
		,	world_widgets = enabled_widgets
		}
	playIO (InWindow "Serenity Mark I" size (0,40)) black 20 start picture (events slice_max) run where
		size = (xmax-xmin, ymax-ymin + 2*bottom)

run :: Float -> World -> IO World
run time_past world = case evolve_mode world of
	Static    -> return $ world'
	Evolving  -> return $ if tick_due then tick_forward world' {last_tick = time world} else world'
	Devolving -> return $ if tick_due then tick_back    world' {last_tick = time world} else world'
	where
		world' = main_time_forward time_past world
		tick_due = (last_tick world + 0.1) < time world


