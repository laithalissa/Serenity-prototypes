module Main where

import Data.Monoid

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Runners.Options
import Test.Framework.Options
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Test.SpaceTime.World
import Test.SpaceTime.Unit
import Test.SpaceTime.History
import Test.SpaceTime.Configuration
import Test.SpaceTime.Graphics
import Test.SpaceTime.Widget
import Test.SpaceTime.Temporal
import Test.SpaceTime.Terrain
import Test.SpaceTime.Util

main = defaultMainWithArgs tests htf_args

htf_args = 
	[	"--maximum-generated-tests=5000"
	,	"--maximum-unsuitable-generated-tests=3000"
	]

tests = 
	[	testProperty "reverse.reverse == id"     prop_reverse_reverse
	,	testProperty "minimum.invert == maximum" prop_min_invert_max
	]

prop_reverse_reverse :: [Char] -> Bool
prop_reverse_reverse s = (reverse . reverse) s == s

prop_min_invert_max :: [Int] -> Property
prop_min_invert_max s = s/=[] ==> minimum (map (\x -> -x) s) == -(maximum s)