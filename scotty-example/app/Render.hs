module Render where
import Codec.Picture
import Shapes


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point0 Point0 (Int,Int)

-- Default window renders a small region around the origin into
-- a 50x50 pixel image
defaultWindow :: Window
defaultWindow = Window (point0 (-1.5) (-1.5)) (point0 (1.5) (1.5)) (500,500)

mixColours :: Drawing -> Drawing -> Colour
mixColours [(_,_,c)] [(_,_,c1)] = averageColours c c1

averageColours :: Colour -> Colour -> Colour
averageColours [(r,g,b)] [(r1,g1,b1)] = [(r+1`div`2,g+g1`div`2,b+b1`div`2)]

combine :: String -> Window -> Drawing -> Drawing -> IO ()
combine path win sh th = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      pixRenderer x y = PixelRGB8 r g b where  [(r,g,b)] = (colorForImage $ mapPoint win (x,y))

      colorForImage :: Point0 -> Colour
      colorForImage p | p `inside` sh && p `inside` th  = mixColours sh th
                      | p `inside` sh = p `colour` sh
                      | p `inside` th = p `colour` th
                      | otherwise     = [(0,0,0)]

mask :: String -> Window -> Drawing -> Drawing -> IO ()
mask path win sh th = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      pixRenderer x y = PixelRGB8 r g b where  [(r,g,b)] = (colorForImage $ mapPoint win (x,y))

      colorForImage :: Point0 -> Colour
      colorForImage p | p `inside` sh && p `inside` th  =p `colour` sh
                      | otherwise     = [(0,0,0)]

-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / (fromIntegral $ n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point0]]
pixels (Window p0 p1 (w,h)) =
  [ [ point0 x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]


-- To make the renderer more efficient I'll write a coordinate-transformer
-- that way the O(n) lookup of locations will become an O(1) calculation of locations

-- Linearly scale a value from the range [a1,a2] to the range [b1,b2]
scaleValue :: Fractional a => (a,a) -> (a,a) -> a -> a
scaleValue (a1,a2) (b1,b2) v = b1 + (v - a1) * (b2-b1) / (a2-a1)

-- Convert a screen-space point into an image-space point
-- in a specific window
mapPoint :: Window -> (Int,Int) -> Point0
mapPoint (Window p0 p1 (w,h)) (x,y) = point0 scaledX scaledY
  where
    scaledX = scaleValue (0,fromIntegral w) (getX p0, getX p1) (fromIntegral x)
    scaledY = scaleValue (0,fromIntegral h) (getY p0, getY p1) (fromIntegral y)

-- Render a drawing into an image, then save into a file
-- This version relates the Shape language coordinates to the pixel coordinates
-- using the scaleValue function which is much faster than the original lookup based code.
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      pixRenderer x y = PixelRGB8 r g b where  [(r,g,b)] = (colorForImage $ mapPoint win (x,y))

      colorForImage :: Point0 -> Colour
      colorForImage p | p `inside` sh = p `colour` sh
                      | otherwise     = [(0,0,0)]

