module Keyboard where

import Data.Set as Set
import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..), KeyState(..))

type Keyboard = Set Key

initKeyboard :: Keyboard
initKeyboard = Set.empty

handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent k Down = Set.insert k
handleKeyEvent k Up   = Set.delete k

isKeyDown :: Keyboard -> Key -> Bool
isKeyDown kb k = Set.member k kb
