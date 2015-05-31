{-# LANGUAGE OverloadedStrings #-}

module Parameters where

import Data.Text.Lazy
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Rest
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id $ do
  middleware logStdoutDev
  rest "/:name" defaultConfig {
    contentTypesProvided = return [("text/html",param "name" >>= html . append "Hello, ")]
  }
