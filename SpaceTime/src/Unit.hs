module Unit where

import Data.Map (Map)
import qualified Data.Map as Map

import Temporal

type UnitID = Int

data Unit = Unit
	{	unit_id      :: Int
	,	player       :: Int
	,	orders       :: Temporal Order
	,	start_loc    :: (Int, Int)
	,	current_loc  :: (Int, Int)
	,	max_hp       :: Int
	,	current_hp   :: Int
	,	current_dest :: Maybe (Int, Int)
	,	planned_path :: [(Int,Int)]
	}
	deriving Show

instance Eq Unit where
	Unit {unit_id=x} == Unit {unit_id=y} = x == y

-- Orders
data Order = 
	  Move Direction 
	| Goto       (Int, Int)
	| Attack     (Int, Int)
	| AttackGoto (Int, Int)
	deriving (Eq, Show)

data Direction =   
	  U | D | L | R | UL | UR | DL | DR
	deriving (Eq, Show)

data Action = 
	  ActionNothing
	| ActionMove Direction 
	| ActionPathfind (Int, Int)
	| ActionAttack Direction 
	deriving (Show)

basic_unit :: Int -> Int -> (Int, Int) -> Unit
basic_unit u_id p (x,y) = Unit
	{	unit_id      = u_id
	,	player       = p
	,	orders       = fromList [(3, Goto (30, 30)), (22, Goto (2,5))] --empty
	,	start_loc    = (x, y)
	,	current_loc  = (x, y)
	,	max_hp       = 3
	,	current_hp   = 3
	,	current_dest = Nothing
	,	planned_path = []
	}