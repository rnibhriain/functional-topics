module Shapes(
  Shape, Point0, Vector, Transform, Drawing, Colour,
  point0, getX, getY, polygon, ellipse,
  empty, circle, square, rectangle, mandelbrotset,
  identity0, translate, rotate, colour, scale, shear, (<+>),
  inside)  where

import Codec.Picture
import Data.Geometry
import Data.Geometry.Polygon
import Data.Geometry.Polygon.Convex
import Data.Ext

-- Utilities

data Vector0 = Vector0 Double Double
              deriving Show
vector0 = Vector0

cross0 :: Vector0-> Vector0-> Double
cross0 (Vector0 a b) (Vector0 a' b') = a * a' + b * b'

mult :: Matrix -> Vector0-> Vector0
mult (Matrix r0 r1) v = Vector0 (cross0 r0 v) (cross0 r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector0 a b) (Vector0 c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector0 Vector0
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector0 a b) (Vector0 c d)

getX (Vector0 x y) = x
getY (Vector0 x y) = y

-- Shapes

type Point0  = Vector0

point0 :: Double -> Double -> Point0
point0 = vector0



data Shape = Empty 
           | Circle 
           | Square
           | Rectangle
           | Ellipse
           | Polygon
           | MandelbrotSet
             deriving Show

empty, circle, square, ellipse, polygon, rectangle, mandelbrotset :: Shape

empty = Empty
circle = Circle
square = Square
ellipse = Ellipse
polygon = Polygon
rectangle = Rectangle
mandelbrotset = MandelbrotSet

-- Transformations

data Transform = Identity0
           | Translate Vector0
           | Scale Vector0
           | Shear Double
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity0 = Identity0
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
shear = Shear
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point0 -> Point0
transform Identity0                   x = id x
transform (Translate (Vector0 tx ty)) (Vector0 px py)  = Vector0 (px - tx) (py - ty)
transform (Scale (Vector0 tx ty))     (Vector0 px py)  = Vector0 (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Shear m) (Vector0 tx ty) = Vector0 (m*ty + tx)  ty
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape,Colour)]

type Colour = [(Pixel8,Pixel8,Pixel8)]

-- interpretation function for drawings

{--}
-- This is a translation of the Mandelbrot set program from
-- Mark Jones' paper.
-- The only meaningful change is that we use the Point type
-- from Shapes, and thus use getX and getY to project out the
-- x and y portions of the point.

-- "next" generates a series of points iteratively
-- If the points in the series end up being "close" to the original point
-- then we declare it to be part of the set, otherwise it is not.
next :: Point0 -> Point0 -> Point0
next p0 p1 = point0 (x * x - y * y + u) (2 * x * y + v)
  where (u, v) = (getX p0, getY p0)
        (x, y) = (getX p1, getY p1)

mandelbrot :: Point0 -> [Point0]
mandelbrot p = iterate (next p) (point0 0 0)

fairlyClose :: Point0 -> Bool
fairlyClose p = (u * u + v * v) < 100
   where (u,v) = (getX p, getY p)

approxTest :: Int -> Point0 -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

colour :: Point0 -> Drawing -> Colour
colour _ [(_,_,c)]  = c

inside :: Point0 -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point0 -> (Transform, Shape, Colour) -> Bool
inside1 p (t,s,_) = insides (transform t p) s

insides :: Point0 -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 0.5
p `insides` Ellipse = distance1 p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` Rectangle = maxnorm1  p <= 1
p `insides` MandelbrotSet = approxTest 100 p
(Vector0 l r) `insides` Polygon = insidePolygon (Point2 l r) simplePoly && isConvex simplePoly

simplePoly :: SimplePolygon ( ) Double
simplePoly = fromPoints $ map ext $
                            [ Point2 0.0 0.25
                            , Point2 0 0.75
                            , Point2 0.25 1
                            , Point2 0.5 1
                            , Point2 0.75 0.75
                            , Point2 0.75 0.25
                            , Point2 0.5 0
                            , Point2 0.25 0
                            ]

distance :: Point0 -> Double
distance (Vector0 x y ) = sqrt ( x**2 + y**2 )

distance1 :: Point0 -> Double
distance1 (Vector0 x y ) = sqrt ( x**2/0.5**2 + y**2/0.3**2 )

maxnorm :: Point0 -> Double
maxnorm (Vector0 x y ) = max (abs x) (abs y)

maxnorm1 :: Point0 -> Double
maxnorm1 (Vector0 x y) = max (abs x / 0.5)  (abs y/ 0.7)
