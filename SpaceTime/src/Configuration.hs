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
xsize_ :: Float
xsize_ = fromIntegral xsize

ysize :: Int
ysize = ymax-ymin
ysize_ :: Float
ysize_ = fromIntegral ysize

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
slice_max :: Int
slice_max = 150
slice_max_ :: Float
slice_max_ = fromIntegral slice_max