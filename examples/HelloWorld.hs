{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Rest
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id $ do
  middleware logStdoutDev
  rest "/" defaultConfig {
    contentTypesProvided = return [("text/html", html "Hello, World!")]
  }
