module Widget (
	Widget(..)
,	WidgetState(..)
,	default_widget
,	widget_at
,	mouse_event
) where

import Configuration
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

class WidgetState world where
	provide_widgets    :: world -> [Widget world]
	get_current_widget :: world -> Maybe (Widget world)
	set_current_widget :: Maybe (Widget world) -> world -> world

	provide_widgets _      = []
	get_current_widget _   = Nothing
	set_current_widget _ a = a

data WidgetState world => Widget world = Widget
	{	bottom_left     :: (Float, Float)
	,	top_right       :: (Float, Float)
	,	mouse_up_cb     :: Event -> world -> IO world
	,	mouse_down_cb   :: Event -> world -> IO world
	,	mouse_motion_cb :: Event -> world -> IO world
	,	widget_picture  :: world -> Picture
	}

default_widget :: WidgetState world => Widget world
default_widget = Widget
	{	bottom_left     = (0,0)
	,	top_right       = (0,0)
	,	mouse_up_cb     = \_ world -> return world
	,	mouse_down_cb   = \_ world -> return world
	,	mouse_motion_cb = \_ world -> return world
	,	widget_picture  = \_       -> Pictures []
	}

widget_at :: WidgetState world => [Widget world] -> (Float,Float) -> Maybe (Widget world)
widget_at widgets (x,y) = case filter (inside (x, y - bottom_)) (reverse widgets) of
	(x:_) -> Just x
	_ -> Nothing

inside :: WidgetState world => (Float, Float) -> Widget world -> Bool
inside (x, y) (Widget {bottom_left = (bx, by), top_right= (tx, ty)} ) = and [x < tx, x > bx, y < ty, y > by]

mouse_event :: WidgetState world => Event -> world -> IO world
mouse_event event @ (EventKey key state (Modifiers Up Up Up) (x,y)) world
		| key == (MouseButton LeftButton) && state == Up   = c_widget event $ (set_current_widget Nothing world)
		| key == (MouseButton LeftButton) && state == Down = down event $ (set_current_widget (Just widget) world)
	where
		widget @ Widget {mouse_down_cb = down} = case widget_at widgets (x,y) of
			Just w -> w
			Nothing -> default_widget
		widgets = provide_widgets world
		c_widget = case get_current_widget world of 
			Just w -> mouse_up_cb w
			Nothing -> mouse_up_cb default_widget
mouse_event event @ (EventMotion _) world = move event world where
	Widget {mouse_motion_cb = move} = case get_current_widget world of
		Just w -> w
		Nothing -> default_widget

