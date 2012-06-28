module Graphics where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array

import World
import Unit
import Configuration
import qualified Temporal as T
import Util

picture :: World -> IO Picture
picture world = 
	return $ 
	Translate (0.0) ((fromIntegral bottom)) $ 
	Pictures 
	[	pictureGridEmpties
	,	pictureGrid (gridOfSlice ((c_history world) !! (slice world))) world
	,	pictureTerrain (terrain world)
	,	pictureButtons
	,	pictureWidgets world
	,	pictureMouseLoc (mouse_coord world)
	]
	where
	gridOfSlice (Slice grid _) = grid

pictureWidgets world = Pictures $ map (\widget -> widget_picture widget $ world) (world_widgets world)

pictureGridEmpties :: Picture
pictureGridEmpties = 
	Translate (fromIntegral xmin) (fromIntegral ymin) $ 
	Pictures 
	[	Translate 
			(fromIntegral (x-1) * s) 
			(fromIntegral (y-1) * s) 
			(Color (greyN 0.2) square) 
		| x<-[1..xbound], y<-[1..ybound]
	]

pictureGrid :: Grid -> World -> Picture
pictureGrid grid (World {slice = ti}) = 
	Translate (fromIntegral xmin) (fromIntegral ymin) $ 
	Pictures $ 
	map drawUnit grid where
		drawUnit (unit @ Unit {current_loc = (x,y)}) = 
			Translate 
			(fromIntegral (x-1) * s) 
			(fromIntegral (y-1) * s) 
			(drawOneUnit unit ti)

drawOneUnit :: Unit -> Int -> Picture
drawOneUnit unit ti = 
	Pictures 
	[	Color orders_color square
	,	Translate (s/4) (s/3) $ Scale (1 / (2 * s)) (1 / (s * 2)) (Text (show (player unit)))
	,	Color black $ Polygon [(0,0),(0,(s-1)/4),(s-1,(s-1)/4),(s-1,0)]
	,	Color green $ Polygon [(0,0),(0,(s-1)/4),((s-1)/health,(s-1)/4),((s-1)/health,0)]
	] 
	where
	orders_color = case (orders unit) T.! ti of
		Nothing -> yellow
		Just a -> case a of
			Move _ -> orange
			Goto _ -> orange
			Attack _ -> red
			AttackGoto _ -> blue
	health = (fromIntegral (current_hp unit)) / (fromIntegral (max_hp unit))

pictureTerrain :: Terrain -> Picture
pictureTerrain terr = 
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

-- GUI

pictureMouseLoc (x, y) = 
	Translate (fromIntegral xmin + 3) (fromIntegral ymin - 53 ) $ 
	Pictures
	[	Color white $ Scale 0.1 0.1 $ Text (show x ++ ", " ++ (show y))
	]

pictureButtons = 
	Translate (fromIntegral xmin + 3) (fromIntegral ymin - 23 ) $ 
	Pictures
	[	pictureSelectButton
	]

pictureSelectButton = Pictures
	[	Color (greyN 0.8) (Scale 5 2 square)
	,	Translate 5 3 $ Scale 0.1 0.1 $ Color black (Text "Select")
	]

mouse :: World -> Event ->  IO World
mouse world event @ (EventKey key state (Modifiers Up Up Up) (x,y)) 
		| key == (MouseButton LeftButton) && state == Up   = 
			(\world -> c_widget event $ world {current_widget = Nothing }) world
		| key == (MouseButton LeftButton) && state == Down = 
			(\world -> down event $ world {current_widget = Just widget }) world
	where
		widget @ Widget {mouse_down_cb = down} = case widget_at widgets (x,y) of
			Just w -> w
			Nothing -> nothing_widget
		widgets = world_widgets world
		c_widget = case current_widget world of 
			Just w -> mouse_up_cb w
			Nothing -> mouse_up_cb nothing_widget

mouse world event @ (EventMotion _) = move event world where
	Widget {mouse_motion_cb = move} = case current_widget world of
		Just w -> w
		Nothing -> nothing_widget

widget_at :: [Widget] -> (Float,Float) -> Maybe Widget
widget_at widgets (x,y) = case filter (inside (x, y)) widgets of
	(x:_) -> Just x
	_ -> Nothing
	where
		inside (x, y) (Widget {bottom_left = (bx, by), top_right= (tx, ty)} ) = and [x < tx, x > bx, y < ty, y > by]

-- Handle control events
events :: Int -> Event -> World -> IO World
events s_max event @ (EventKey key state (Modifiers Up Up Up) (x,y)) world @ World {time=t, slice=sl, evolving = e}
	| key == control_left  = if state == Up then return $ world { evolving = Static } else return $ world { evolving = Devolving }
	| key == control_right = if state == Up then return $ world { evolving = Static } else return $ world { evolving = Evolving  }
	| key == control_evolve && state == Up = return $ world { evolving = if e == Evolving then Static else Evolving }
	| key == (MouseButton LeftButton) = mouse world event
	-- | key == (MouseButton LeftButton) && state == Up   = return $ click x y (world {mouse_l_down = False})
	-- | key == (MouseButton LeftButton) && state == Down = return $ world {mouse_l_down = True}
events s_max event @ (EventMotion (x, y)) world @ World {time=t, slice=sl, evolving = e} =
	mouse world' event where
	--if mouse_l_down world then return $ click x y world' else return $ world' where
		world' = world {mouse_coord = (x,y)}
events _ _ x = return x

-- Timeline Widget
timeline_widget :: Widget
timeline_widget = Widget
	{	bottom_left     = (slider_coord 0           , ymin_ + bottom_ / 2 - 2*s)
	,	top_right       = (slider_coord (sliceMax-1), ymin_ + bottom_ / 2 - s  )
	,	mouse_up_cb     = \_ world -> return world
	,	mouse_down_cb   = \(EventKey _ _ _ (x,y)) world -> return world {slice = toBounds (floor $ slider_position x) (0, sliceMax-1)}
	,	mouse_motion_cb = \(EventMotion (x, y))   world -> return world {slice = toBounds (floor $ slider_position x) (0, sliceMax-1)}
	,	widget_picture  = \world -> pictureTimeline world
	} where
		slider_coord x = (*) (fromIntegral sliceMax / (fromIntegral (xsize))) $ (-(s * (fromIntegral sliceMax)/2)) + (fromIntegral (x) * s)
		slider_position x = (/) ((x * (fromIntegral (xsize) / (fromIntegral sliceMax)) + (s * (fromIntegral sliceMax)/2)) ) s

pictureTimeline :: World -> Picture
pictureTimeline world = 
	Scale (fromIntegral sliceMax / (fromIntegral (xsize))) 1.0 $ 
	Translate (-(s * (fromIntegral sliceMax)/2)) (fromIntegral (ymin-bottom)) $ 
	Pictures 
	[ 	Translate 
			(fromIntegral (i) * s) 0
			(Color (if (slice world) == i then red else (greyN 0.7)) square')
		| i<-[0..(sliceMax-1)]
	]

enabled_widgets :: [Widget]
enabled_widgets = 
	[	timeline_widget
	]

--click x y world
--	| and [x < xmax_ , x > xmin_, y < ymax_+ bottom_, y > ymin_+ bottom_] = world
--	| and [y < timeline_y_max, y > timeline_y_min] = world {slice = toBounds (floor i) (0, sliceMax-1)}
--	| otherwise = world
--	where 
--		i = (/) ((x * (fromIntegral (xsize) / (fromIntegral sliceMax)) + (s * (fromIntegral sliceMax)/2)) ) s
--		timeline_y_max = ymin_ + bottom_ / 2 - s
--		timeline_y_min = ymin_ + bottom_ / 2 - 2*s


-- Controls
control_left   :: Key
control_right  :: Key
control_evolve :: Key
control_left   = SpecialKey KeyLeft
control_right  = SpecialKey KeyRight
control_evolve = SpecialKey KeySpace
