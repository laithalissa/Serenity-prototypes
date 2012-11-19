{-# LANGUAGE Arrows #-}

module Game where

import Control.Wire
import Control.Wire.Wire
import Data.List (foldl')
import Data.Monoid
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..), KeyState(..))
import Prelude hiding ((.), id)

import Keyboard
import Models

type GameWire = WireP Keyboard [Picture]

game :: GameWire
game = proc kb -> do
	paddle <- paddleWire -< kb
	-- ball <- ballWire -< paddle
	returnA -< [draw (Paddle paddle)] -- , draw (Ball ball)]

paddleWire :: WireP Keyboard PaddlePos
paddleWire = proc kb -> do
	spd <- paddleSpeedWire -< kb
	x <- accum (+) paddleInitPos -< spd
	returnA -< x

paddleSpeedWire :: WireP Keyboard Int
paddleSpeedWire = arr keyboardDir where
	keyboardDir kb
		| isKeyDown kb (SpecialKey KeyLeft) = -paddleSpeed
		| isKeyDown kb (SpecialKey KeyRight) = paddleSpeed
		| otherwise = 0

ballWire :: WireP PaddlePos BallPos
ballWire = proc paddle -> do
	rec	bnc <- bounceWire -< (paddle, ball)
		vel <- accum bounce ballInitVel -< bnc
		ball <- delay ballInitPos <<< accum (+) ballInitPos -< vel

	returnA -< ball

bounceWire :: WireP (PaddlePos, BallPos) BallBounce
bounceWire = padBounce <|> wallBounce <|> constant NoBounce

padBounce :: WireP (PaddlePos, BallPos) BallBounce
padBounce = (constant VBounce) . when collision

collision :: (PaddlePos, BallPos) -> Bool
collision (paddle, (bx, by)) = bx >= paddle && bx <= paddle + paddleWidth && by <= paddleHeight

wallBounce :: WireP (PaddlePos, BallPos) BallBounce
wallBounce = (constant HBounce) . when (\(_, (x, _)) -> x < leftWall || x > rightWall) <|>
	(constant VBounce) . when (\(_, (_, y)) -> y > ceilingHeight || y <= 0) -- XXXX
