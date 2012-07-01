module SpaceTime.Temporal (
	Temporal
,	empty
,	(!)
,	insert
,	clear_from
,	fromList
,	toList
) 
where

import Data.Map (Map)
import qualified Data.Map as Map

newtype Temporal a = T (Map Int a) deriving Show

empty :: Temporal a
empty = T Map.empty

(!) :: Temporal a -> Int -> Maybe a
(!) (T t) i = case (Map.lookup i t) of
	Just x -> Just x
	Nothing -> case i of
		0 -> Nothing
		_ -> (T t) ! (i-1)

insert :: Temporal a -> Int -> a -> Temporal a
insert (T t) i a = T (Map.insert i a t)

clear_from :: Temporal a -> Int -> Temporal a
clear_from (T t) x = T (Map.filterWithKey (\k _ -> k < x) t)

toList ::  Temporal a -> [(Int, a)]
toList (T t) = Map.toList t

fromList :: [(Int, a)] -> Temporal a
fromList t = T (Map.fromList t)