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
exampleDrawing1 =  [ (translate (point 0.5 0.25), square, [(255, 0, 0)]) ]

-- mandlebrot
exampleDrawing2 = [ (identity, mandelbrotset, [(255, 0, 0)])]

-- ellipse
exampleDrawing3 =  [ (identity, ellipse,[(255, 0, 0)]) ]

-- rectangle
exampleDrawing4 = [ (identity, rectangle, [(255, 0, 0)])]

-- polygon
exampleDrawing5 = [ (identity, mandelbrotset, [(255, 0, 0)])]

main :: IO ()
main = do 
        render "ellipse.png" defaultWindow exampleDrawing3
        render "mandlebrot.png" defaultWindow exampleDrawing2
        render "square.png" defaultWindow exampleDrawing3
        render "circle.png" defaultWindow exampleDrawing
        render "rectangle.png" defaultWindow exampleDrawing4
        main2

main2 :: IO () 
main2 = 
  scotty 3000 $ do
  get "/" $ do
    html "donagh is a big eejit"
  get "/greet/" $ do
      html  "Hello there"
  get "/greet/:name" $ do
      name <- param "name"
      html $ response name
  get "/hello/:person" $ do 
      person <- param "person"
      html $ longresponse person
  get "/ellipse" $ do
          file "./ellipse.png"
  get "/rectangle" $ do
          file "./rectangle.png"
  get "/square" $ do
          file "./square.png"
  get "/mandlebrot" $ do
          file "./mandlebrot.png"
  get "/circle" $ do
          file "./circle.png"
          --- uhtml "exampleDrawing =  [ (shear 1, circle, [(255, 0, 255)]) ]"


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