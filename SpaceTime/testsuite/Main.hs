module Main where

import Test.QuickCheck (quickCheck)
import Text.Printf

import Test.SpaceTime.World
import Test.SpaceTime.Unit
import Test.SpaceTime.History
import Test.SpaceTime.Configuration
import Test.SpaceTime.Graphics
import Test.SpaceTime.Widget
import Test.SpaceTime.Temporal
import Test.SpaceTime.Terrain
import Test.SpaceTime.Util

prop_reverseReverse :: [Char] -> Bool
prop_reverseReverse s = (reverse . reverse) s == s

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests = 
	[	("reverse.reverse/id", quickCheck prop_reverseReverse)
	,	("reverse.reverse/id", quickCheck prop_reverseReverse)
	]