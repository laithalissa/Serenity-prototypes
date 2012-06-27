module Util where

minimumWith :: Ord b => (a -> b) -> [a] -> a
minimumWith _ [] =  error "Empty List: minimumWith"
minimumWith f xs =  foldl1 (minWith f) xs

minWith :: Ord b => (a -> b) -> a -> a -> a
minWith f x y = if f x <= f y then x else y

toBounds i (x,y) 
	| i < x     = x
	| i > y     = y
	| otherwise = i