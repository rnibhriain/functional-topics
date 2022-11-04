module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]
exampleDrawing2 = [ (identity, mandelbrotset)]

main = render "output.png" defaultWindow exampleDrawing2
