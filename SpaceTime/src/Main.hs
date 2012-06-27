module Main (
	main
) 
where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.ParserCombinators.Parsec
import Data.Array
import Control.DeepSeq
import Data.Map (Map)
import qualified Data.Map as Map

import World
import Unit
import History
import Configuration
import Graphics
import Terrain

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
	playIO (InWindow "Serenity Mark I" size (0,0)) black 20 start picture (events sliceMax) run where
		size = (xmax-xmin, ymax-ymin + 2*bottom)

run :: Float -> World -> IO World
run f (world @ World {time = t, last_tick = l_tick, evolving = evolve}) = 
	if evolve 
		then return $ tick_if_time
		else return $ advance_main_time
	where
	tick_if_time = if (l_tick + 0.1) < t then advance_main_time_and_slice else advance_main_time
	advance_main_time = main_time_forward f world
	advance_main_time_and_slice = tick_forward $ (main_time_forward f world) {last_tick = f+t}
