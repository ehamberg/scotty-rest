{-# LANGUAGE OverloadedStrings #-}

module HelloWorld where

import Web.Scotty.Trans
import Web.Scotty.Rest
import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = scottyT 7000 id id $ do
  middleware logStdoutDev
  rest "/" defaultConfig {
    contentTypesProvided = return [("text/html",html "Hello, World!")]
  }
