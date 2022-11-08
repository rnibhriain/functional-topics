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

exampleDrawing =  [ (translate (point 0.5 0.25), circle) ]
exampleDrawing1 =  [ (translate (point 0.5 0.25), square) ]
exampleDrawing2 = [ (identity, mandelbrotset)]

main :: IO ()
main = do 
        render "mandlebrot.png" defaultWindow exampleDrawing2
        render "square.png" defaultWindow exampleDrawing1
        render "circle.png" defaultWindow exampleDrawing
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
  get "/square" $ do
          file "./square.png"
  get "/mandlebrot" $ do
          file "./mandlebrot.png"
  get "/circle" $ do
          file "./circle.png"


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