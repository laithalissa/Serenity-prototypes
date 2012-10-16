module SpaceTime.History where

import Data.List
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph.AStar
import Control.DeepSeq

import SpaceTime.Configuration
import SpaceTime.Unit
import qualified SpaceTime.Temporal as T
import SpaceTime.World

history :: World -> [Slice]
history world = take slice_max $ iterate (step (terrain world)) init_slice where
	init_slice = Slice (init_grid world) 1

step :: Terrain -> Slice -> Slice
step terr (Slice grid ti) = Slice new_grid (ti+1) where
	new_grid = map fst outcomes
	outcomes = map (act terr grid ti) grid

act :: Terrain -> Grid -> Time -> Unit -> (Unit, Maybe (Int, Int))
act terr grid ti unit = (new_unit, Nothing) where
	unit_orders = (orders unit) T.! ti
	action = decide_action terr grid ti unit
	old_location = current_loc unit
	new_location = case action of
		ActionMove dir -> translate old_location dir
		ActionPathfind dest -> case p of
			Just (a:as) -> a
			Just [] -> old_location
			Nothing -> old_location
		_  -> old_location
	new_dest = case unit_orders of
		Just o -> case o of
			Goto loc -> Just loc
			Attack loc -> Just loc
			AttackGoto loc -> Just loc
			_ -> current_dest unit
		Nothing -> current_dest unit
	new_path = case action of
		ActionPathfind dest -> case p of
			Just a -> tail a
			Nothing -> []
		ActionMove dir -> case planned_path unit of --tail $ planned_path unit
			_:x -> x
			_ -> []
		ActionNothing -> planned_path unit
		--ActionAttack _
	p = case new_dest of 
		Just n_dest -> pathfind terr grid old_location n_dest
		Nothing -> Nothing
	new_unit = unit
		{	current_dest = new_dest
		,	current_loc = new_location
		,	planned_path = new_path
		}

decide_action :: Terrain -> Grid -> Time -> Unit -> Action
decide_action terr grid ti unit = 
	case order of
		Nothing -> ActionNothing
		Just (Move dir) -> ActionMove dir
		Just (Goto loc) -> if (Just (current_loc unit) == current_dest unit) 
			then ActionNothing 
			else case (current_dest unit) of
				Just c_dest -> if c_dest == loc 
					then if (planned_path unit == []) 
						then ActionNothing 
						else ActionMove $ get_direction (current_loc unit) ((planned_path unit)!!0) 
					else ActionPathfind loc
				Nothing -> ActionPathfind loc
		--Just (Attack loc)
		--Just (AttackGoto loc)
	where
		order = (T.!) (orders unit) ti

pathfind :: Terrain -> Grid -> (Int, Int) -> (Int, Int) -> Maybe [(Int,Int)]
pathfind  terr grid current destination = 
	case p of
		Just [] -> Nothing
		Just a -> Just a
		Nothing -> Nothing --pathfind terr grid (move_towards current destination) current
	where 
		p = path terr grid destination current
		--move_towards (x1,y1) (x, y) = (x2, y2) where
		--	x2 = if x1 > x then x+1 else x-1
		--	y2 = if y2 > y then y+1 else y-1

-- A* pathfinding using Data.Graph.AStar

path :: Terrain -> Grid -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
path terr grid dest current = aStar (graph terr grid) distance (manhattan dest) (== dest) current

directions :: [Direction]
directions = [U,D,L,R,UL,UR,DL,DR]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (ax,ay) (bx,by) = abs (ax - bx) + abs (ay - by)

translate :: (Int, Int) -> Direction -> (Int, Int)
translate (x,y) dir = case dir of
	U  -> (x  ,y+1)
	D  -> (x  ,y-1)
	L  -> (x-1,y  )
	R  -> (x+1,y  )
	UL -> (x-1,y+1)
	UR -> (x+1,y+1)
	DL -> (x-1,y-1)
	DR -> (x+1,y-1)

get_direction :: (Int, Int) -> (Int, Int) -> Direction
get_direction (x,y) (x1,y1)
	| (x1,y1) == (x  ,y+1) = U
	| (x1,y1) == (x  ,y-1) = D
	| (x1,y1) == (x-1,y  ) = L
	| (x1,y1) == (x+1,y  ) = R
	| (x1,y1) == (x-1,y+1) = UL
	| (x1,y1) == (x+1,y+1) = UR
	| (x1,y1) == (x-1,y-1) = DL
	| (x1,y1) == (x+1,y-1) = DR
	| otherwise            = error "direction not one away"

graph :: Terrain -> Grid -> (Int, Int) -> Set (Int,Int)
graph terr grid (c_x, c_y) = Set.fromList reachable where 
	reachable    = filter allowed all_neighbours
	all_neighbours = map (\d -> translate (c_x,c_y) d) directions'
	allowed (x,y)  = and 
		[	x >= 0
		,	y >= 0
		,	x <= xbound
		,	y <= ybound
		,	terr ! x ! y
		--,	not $ (x,y) `elem` (map current_loc) grid
		] 
	directions' = directions \\ unreachable where
		unreachable = 
			(if terr!(x1)!(c_y) then [] else [DR, UR]) ++
			(if terr!(x2)!(c_y) then [] else [UL, DL]) ++
			(if terr!(c_x)!(y1) then [] else [UL, UR]) ++
			(if terr!(c_x)!(y2) then [] else [DL, DR])
	x1 = if c_x==xbound then xbound else c_x+1
	x2 = if c_x==0 then 0 else c_x-1
	y1 = if c_y==ybound then ybound else c_y+1
	y2 = if c_y==0 then 0 else c_y-1

distance :: (Int, Int) -> (Int, Int) -> Int
distance a b = case (get_direction a b) of
	U  -> 10
	D  -> 10
	L  -> 10
	R  -> 10
	UL -> 14
	UR -> 14
	DL -> 14
	DR -> 14

