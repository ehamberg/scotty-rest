{-# LANGUAGE OverloadedStrings #-}

module Parameters where

import Web.Scotty.Trans
import Web.Scotty.Rest
import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy

main :: IO ()
main = scottyT 7000 id id $ do
  middleware logStdoutDev
  rest "/:name" defaultConfig {
    contentTypesProvided = return [("text/html",param "name" >>= html . append "Hello, ")]
  }
