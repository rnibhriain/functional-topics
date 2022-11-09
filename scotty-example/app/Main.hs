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

-- circle
exampleDrawing =  [ (shear 1, circle, [(255, 0, 255)]) ]

-- square
exampleDrawing1 =  [ (translate (point0 0.5 0.25), square, [(255, 0, 0)]) ]

-- mandlebrot
exampleDrawing2 = [ (identity0, mandelbrotset, [(255, 0, 255)])]

-- ellipse
exampleDrawing3 =  [ (identity0, ellipse,[(255, 100, 0)]) ]

-- rectangle
exampleDrawing4 = [ (identity0, rectangle, [(255, 0, 50)])]

-- polygon
exampleDrawing5 = [ (shear 1, polygon, [(255, 255, 0)])]

main :: IO ()
main = do 
        render "circle.png" defaultWindow exampleDrawing
        render "square.png" defaultWindow exampleDrawing1
        render "mandlebrot.png" defaultWindow exampleDrawing2
        render "ellipse.png" defaultWindow exampleDrawing3
        render "rectangle.png" defaultWindow exampleDrawing4
        render "polygon.png" defaultWindow exampleDrawing5
        runServer

runServer :: IO () 
runServer = 
  scotty 3000 $ do
  get "/greet/" $ do
      html  "Hello there"
  get "/greet/:name" $ do
      name <- param "name"
      html $ response name
  get "/hello/:person" $ do 
      person <- param "person"
      html $ longresponse person
  get "/circle" $ do
          file "./circle.png"
  get "/square" $ do
          file "./square.png"
  get "/mandlebrot" $ do
          file "./mandlebrot.png"
  get "/ellipse" $ do
          file "./ellipse.png"
  get "/rectangle" $ do
          file "./rectangle.png"
  get "/polygon" $ do
          file "./polygon.png"


response :: Text -> Text
response n = do R.renderHtml $ do
                  H.h1 ( "Hello " >> H.toHtml n)

longresponse :: Text -> Text
longresponse n = do
  R.renderHtml $ do
    H.head $ H.title "Welcome page"
    H.body $ do
      H.h1 "Welcome!"
      H.p ("Welcome to my Scotty app " >> H.toHtml n)

myImage :: H.Html
myImage = H.img H.! A.src "output.png" H.! A.alt "Hm." 