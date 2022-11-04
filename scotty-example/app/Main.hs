{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R


main :: IO ()
main = scotty 3000 $ do
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
  get "/cat" $ do
          html $ R.renderHtml
            myImage 
  get "/image" $ do
          file "./output.png"


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
myImage = H.img H.! A.src "catpicture.jpg" H.! A.alt "Hm." 