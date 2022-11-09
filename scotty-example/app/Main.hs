{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Codec.Picture
import Shapes
import Render
import qualified Text.Blaze.Html4.FrameSet as F
import qualified Control.Applicative as H
import qualified GHC.Read as H

-- circle
circlePic =  [ (shear 1, circle, [(255, 0, 255)]) ]
circlePic1 =  [ (identity0, circle, [(0, 100, 255)]) ]
circlePic2 =  [ (translate (point0 0.5 0), circle, [(255, 0, 255)]) ]

-- square
squarePic =  [ (translate (point0 0.5 0.25), square, [(255, 0, 0)]) ]

-- mandlebrot
mandlebrotPic = [ (rotate 7, mandelbrotset, [(255, 0, 255)])]

-- ellipse
ellipsePic =  [ (identity0, ellipse,[(255, 100, 0)]) ]

-- rectangle
rectanglePic = [ (shear 1, rectangle, [(255, 0, 50)])]

-- polygon
polygonPic = [ (scale (point0 1.5 1.5), polygon, [(255, 255, 0)])]

main :: IO ()
main = do 
        combine "combining.png" defaultWindow circlePic1 circlePic2
        mask "masking.png" defaultWindow circlePic1 circlePic2
        render "circle.png" defaultWindow circlePic
        render "square.png" defaultWindow squarePic
        render "mandlebrot.png" defaultWindow mandlebrotPic
        render "ellipse.png" defaultWindow ellipsePic
        render "rectangle.png" defaultWindow rectanglePic
        render "polygon.png" defaultWindow polygonPic
        runServer

runServer :: IO () 
runServer = 
  scotty 3000 $ do
  get "/" $ do
      html  response
  get "/circle" $ do
         setHeader "Content-Type" "image/png"
         file "./circle.png"
  get "/combining" $ do
         setHeader "Content-Type" "image/png"
         file "./combining.png"
  get "/square" $ do
          setHeader "Content-Type" "image/png"
          file "./square.png"
  get "/mandlebrot" $ do
          setHeader "Content-Type" "image/png"
          file "./mandlebrot.png"
  get "/ellipse" $ do
          setHeader "Content-Type" "image/png"
          file "./ellipse.png"
  get "/rectangle" $ do
          setHeader "Content-Type" "image/png"
          file "./rectangle.png"
  get "/polygon" $ do
          setHeader "Content-Type" "image/png"
          file "./polygon.png"
  get "/masking" $ do
          setHeader "Content-Type" "image/png"
          file "./masking.png"


response :: Text
response = R.renderHtml $ do
    H.head $ H.title "Welcome page"
    H.body $ do
      H.h1 "Welcome!"
      H.h2 "Welcome to my Scotty Image Server "
      H.hr
      H.ul  $ do
         H.li "Circle: [ (shear 1, circle, [(255, 0, 255)]) ]"
         H.li "Square: [ (translate (point0 0.5 0.25), square, [(255, 0, 0)]) ]"
         H.li "Mandlebrot: [ (identity0, mandelbrotset, [(255, 0, 255)])]"
         H.li "Ellipse: [ (identity0, ellipse,[(255, 100, 0)]) ]"
         H.li "Rectangle: [ (shear 1, rectangle, [(255, 0, 50)])]"
         H.li "Polygon: [ (rotate 7, polygon, [(255, 255, 0)])]"
         H.li "Combining: [ (identity0, circle, [(0, 100, 255)]) ] with [ (translate (point0 0.5 0), circle, [(255, 0, 255)]) ]"
         H.li "Masking: [ (identity0, circle, [(0, 100, 255)]) ] with [ (translate (point0 0.5 0), circle, [(255, 0, 255)]) ]"
