module Configuration where

-- Dimensions
xmin :: Int
xmax :: Int
ymin :: Int
ymax :: Int

xmin = -300
xmax =  300
ymin = -300
ymax =  300

xmin_ :: Float
xmax_ :: Float
ymin_ :: Float
ymax_ :: Float

xmin_ = fromIntegral xmin
xmax_ = fromIntegral xmax
ymin_ = fromIntegral ymin
ymax_ = fromIntegral ymax

xsize :: Int
xsize = xmax-xmin

ysize :: Int
ysize = ymax-ymin


bottom :: Int
bottom = 40
bottom_ :: Float
bottom_ = fromIntegral bottom

-- Grid bounds
xbound :: Int
ybound :: Int

xbound = 59
ybound = 59

--Slices
sliceMax :: Int
sliceMax = 150