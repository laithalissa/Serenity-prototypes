module Graphics where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array
import Control.DeepSeq
import qualified Data.Map as Map

import World
import Widget
import History
import Unit
import Configuration
import qualified Temporal as T
import Util

picture :: World -> IO Picture
picture world = 
	return $ 
	Translate (0.0) ((fromIntegral bottom)) $ 
	Pictures 
	[	picture_grid_empties
	,	picture_terrain       (terrain world)
	,	picture_grid          (gridOfSlice ((c_history world) !! (slice world))) world
	,	picture_widgets world
	,	picture_mouse_coords  (mouse_coord world)
	,	picture_orders world  (slice world)
	]
	where
	gridOfSlice (Slice grid _) = grid

picture_widgets world = Pictures $ map (\widget -> widget_picture widget $ world) (world_widgets world)

enabled_widgets :: [Widget World]
enabled_widgets = 
	[	timeline_widget
	,	select_button
	,	attack_button
	,	move_button
	,	game_area_widget
	]

-- Handle control events
events :: Int -> Event -> World -> IO World
events s_max event @ (EventKey key state (Modifiers Up Up Up) (x,y)) world @ World {time=t, slice=sl, evolve_mode  = e}
	| key == control_left  = if state == Up then return $ world {evolve_mode = Static} else return $ world {evolve_mode = Devolving}
	| key == control_right = if state == Up then return $ world {evolve_mode = Static} else return $ world {evolve_mode = Evolving }
	| key == control_evolve && state == Up = return $ world { evolve_mode  = if e == Evolving then Static else Evolving }
	| key == (MouseButton LeftButton) = mouse_event event world
events s_max event @ (EventMotion (x, y)) world @ World {time=t, slice=sl, evolve_mode  = e} =
	mouse_event event world' where
		world' = world {mouse_coord = (x,y)}
events _ _ x = return x

-- Main Graphics
picture_grid_empties :: Picture
picture_grid_empties = 
	Translate (fromIntegral xmin) (fromIntegral ymin) $ 
	Pictures 
	[	Translate 
			(fromIntegral (x-1) * s) 
			(fromIntegral (y-1) * s) 
			(Color (greyN 0.2) square) 
		| x<-[1..xbound], y<-[1..ybound]
	]

picture_grid :: Grid -> World -> Picture
picture_grid grid (world @ World {slice = ti}) = 
	Translate (fromIntegral xmin) (fromIntegral ymin) $ 
	Pictures $ 
	map drawUnit grid where
		drawUnit (unit @ Unit {current_loc = (x,y)}) = 
			Translate 
			(fromIntegral (x-1) * s) 
			(fromIntegral (y-1) * s) 
			(picture_unit world unit ti)

picture_unit :: World -> Unit -> Time -> Picture
picture_unit world unit ti = 
	Pictures $ 
	(if (unit_id unit) `elem` (selected world) then [picture_select] else []) ++
	[	Color orders_color square
	,	Translate (s/4) (s/3) $ Scale (1 / (2 * s)) (1 / (s * 2)) (Text (show (player unit)))
	,	Color black $ Polygon [(0,0),(0,(s-1)/4),(s-1,(s-1)/4),(s-1,0)]
	,	Color green $ Polygon [(0,0),(0,(s-1)/4),((s-1)/health,(s-1)/4),((s-1)/health,0)]
	] 
	where
	orders_color = case (orders unit) T.! ti of
		Nothing -> yellow
		Just a -> case a of
			Goto _ -> orange
			Attack _ -> red
			AttackGoto _ -> blue
	health = (fromIntegral (current_hp unit)) / (fromIntegral (max_hp unit))
	picture_select = Pictures $ 
		[	Color red $ Polygon [(-2,-2),(-2,s+2),(s+2,s+2),(s+2,-2)]
		,	Color black square'
		]

picture_orders :: World -> Time -> Picture
picture_orders world ti = 
	Translate xmin_ ymin_ $ 
	Pictures $ map 
	(\order -> Translate (trans $ fst (orders_loc order)) (trans $ snd (orders_loc order)) $ Color (orders_color order) $ (Circle 10) ) 
	list_orders where
		list_orders = concatMap (\unit -> (get_orders unit)) selected_units
		get_orders unit = case (orders unit) T.! ti of Just a -> [a]; otherwise -> []
		units_now = (\(Slice grid time) -> grid) $ (c_history world) !! (slice world)
		selected_units = filter (\unit -> (unit_id unit) `elem` (selected world)) units_now
		trans a = (fromIntegral (a-1) * s ) + 0.5 * s
		orders_color order = case order of
			Goto _       -> orange
			Attack _     -> red
			AttackGoto _ -> blue
		orders_loc order = case order of
			Goto loc       -> loc
			Attack loc     -> loc
			AttackGoto loc -> loc

picture_terrain :: Terrain -> Picture
picture_terrain terr = 
	Translate (fromIntegral xmin) (fromIntegral ymin) $ 
	Pictures $ concat 
	[	if (terr ! x ! y) == False then [Translate 
			(fromIntegral (x-1) * s) 
			(fromIntegral (y-1) * s) 
			(Color white square)
			]
		else []
		| x<-[1..xbound], y<-[1..ybound]
	]

-- s is the size of a single grid square edge, depending on the grid size
s :: Float
s = (fromIntegral xsize) / (fromIntegral xbound)

square :: Picture
square = Polygon [(0,0),(0,s-1),(s-1,s-1),(s-1,0)]
square' :: Picture
square' = Polygon [(0,0),(0,s),(s,s),(s,0)]

square_of_size x = Polygon [(0,0),(0,x),(x,x),(x,0)]

-- GUI

picture_mouse_coords (x, y) = 
	Translate (fromIntegral xmin + 3) (fromIntegral ymin - 53 ) $ 
	Pictures
	[	Color white $ Scale 0.1 0.1 $ Text (show x ++ ", " ++ (show y))
	]

-- Timeline Widget
timeline_widget :: Widget World
timeline_widget = Widget
	{	bottom_left     = (slider_coord 0           , (ymin_ + bottom_ / 2 - 2*s) - 40 )
	,	top_right       = (slider_coord (slice_max-1), (ymin_ + bottom_ / 2 - s  ) - 40 )
	,	mouse_up_cb     = \_ world -> return world
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world {slice = toBounds (floor $ slider_position x) (0, slice_max-1)}
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world {slice = toBounds (floor $ slider_position x) (0, slice_max-1)}
	,	widget_picture  = \world -> picture_timeline world
	} where
		slider_coord x = (*) (slice_max_ / xsize_) $ (-(s * (slice_max_)/2)) + (fromIntegral (x) * s)
		slider_position x = (/) ((x * (xsize_ / (slice_max_)) + (s * (slice_max_)/2)) ) s

picture_timeline :: World -> Picture
picture_timeline world = 
	Scale (slice_max_ / xsize_) 1.0 $ 
	Translate (-(s * slice_max_/2)) (ymin_-bottom_) $ 
	Pictures 
	[ 	Translate 
			(fromIntegral (i) * s) 0
			(Color (if (slice world) == i then red else (greyN 0.7)) square')
		| i<-[0..(slice_max-1)]
	]

-- Button Widgets

select_button :: Widget World
select_button = Widget
	{	bottom_left     = (xmin_ + 3 , ymin_ - 23)
	,	top_right       = (xmin_ + 3 + 50, ymin_ - 23 + 20)
	,	mouse_up_cb     = \_ world -> return world {mode = ModeSelect}
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world 
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world 
	,	widget_picture  = \world -> 
			Translate (xmin_ + 3) (ymin_ - 23 ) $ 
			picture_button "Select" (active world) world
	}
	where active world = (mode world) == ModeSelect

move_button :: Widget World
move_button = Widget
	{	bottom_left     = (xmin_ + 3 + 50, ymin_ - 23)
	,	top_right       = (xmin_ + 3 + 100, ymin_ - 23 + 20)
	,	mouse_up_cb     = \_ world -> return world {mode = ModeMove}
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world 
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world 
	,	widget_picture  = \world -> 
			Translate (xmin_ + 53) (ymin_ - 23 ) $ 
			picture_button "Move" (active world) world
	}
	where active world = (mode world) == ModeMove

attack_button :: Widget World
attack_button = Widget
	{	bottom_left     = (xmin_ + 3 + 100, ymin_ - 23)
	,	top_right       = (xmin_ + 3 + 150, ymin_ - 23 + 20)
	,	mouse_up_cb     = \_ world -> return world {mode = ModeAttack}
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world 
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world 
	,	widget_picture  = \world -> 
			Translate (xmin_ + 103) (ymin_ - 23 ) $ 
			picture_button "Attack" (active world) world
	}
	where active world = (mode world) == ModeAttack

picture_button label active world = Pictures
	[	Color color (Scale 5 2 (square_of_size 9))
	,	Translate 5 3 $ Scale 0.1 0.1 $ Color black (Text label)
	]
	where
		color_down = greyN 0.9
		color_up = greyN 0.5
		color_active = greyN 0.2
		color = if active then color_down else color_up

game_area_widget :: Widget World
game_area_widget = Widget
	{	bottom_left     = (xmin_, ymin_)
	,	top_right       = (xmax_, ymax_)
	,	mouse_up_cb     = \(EventKey _ _ _ (x,y)) world -> return $ click (x, y) world
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world 
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world 
	,	widget_picture  = \_ -> Pictures []
	} where 
		click (x_, y_) world = case mode world of
			ModeSelect     -> world {selected = map unit_id clicked_units}
			ModeMove       -> world' {c_history = force $ history world', mode = ModeSelect} where 
				world' = world {units = Map.map update_move_orders (units world)}
			ModeAttack     -> world
			ModeAttackMove -> world
			where 
				clicked_units = filter (\unit -> current_loc unit == (x,y)) units_now
				x = floor $ ((x_ - xmin_) / s) + 1
				y = floor $ ((y_ - ymin_ - bottom_) / s) + 1
				units_now = (\(Slice grid time) -> grid) $ (c_history world) !! (slice world)
				update_move_orders unit = if (unit_id unit) `elem` (selected world) 
					then unit {orders = T.insert (orders unit) (slice world) (Goto (x,y))}
					else unit

-- Controls
control_left   :: Key
control_right  :: Key
control_evolve :: Key
control_left   = SpecialKey KeyLeft
control_right  = SpecialKey KeyRight
control_evolve = SpecialKey KeySpace
