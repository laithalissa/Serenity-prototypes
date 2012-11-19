module Models where

import Graphics.Gloss.Data.Picture

leftWall = 0 :: Int
rightWall = 600 :: Int
ceilingHeight = 400 :: Int

type PaddlePos = Int

data Paddle = Paddle { paddlePos :: PaddlePos }

paddleInitPos = 0 :: Int
paddleWidth = 20 :: Int
paddleHeight = 10 :: Int
paddleSpeed = 25 :: Int

data Ball = Ball { ballPos :: BallPos }

type BallPos  = (Int, Int)
type Velocity = (Int, Int)

ballInitPos = (300, 200) :: (Int, Int)
ballRadius  = 8
ballInitVel = (-6, -6) :: (Int, Int)

-- Ball bounce events for horizontal and vertical bounce
data BallBounce = HBounce | VBounce | NoBounce deriving (Show)

-- Multiple a vector by a scalar
vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x,y) = (x*c,y*c)

-- Add two vectors
vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a,b) (c,d) = (a+c,b+d)

-- Adjust velocity based on a bounce event
bounce :: Velocity -> BallBounce -> Velocity
bounce (dx,dy) b = case b of
	HBounce -> (-dx,dy)
	VBounce -> (dx,-dy)
	NoBounce -> (dx, dy)

class Drawable a where
	draw :: a -> Picture

instance Drawable Paddle where
	draw paddle = Polygon [(x, 0), (x, ph), (x + pw, ph), (x + pw, 0)]
		where
			x = fromIntegral $ paddlePos paddle
			pw = fromIntegral paddleWidth
			ph = fromIntegral paddleHeight

instance Drawable Ball where
	draw ball = Translate (fromIntegral x) (fromIntegral y) $ Circle ballRadius
		where (x, y) = ballPos ball
