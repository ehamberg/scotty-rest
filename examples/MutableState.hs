{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text.Lazy (pack)
import Web.Scotty.Trans (scottyT, html)
import Web.Scotty.Rest

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  scottyT 3000 id $ do
    rest "/" defaultConfig {
      contentTypesProvided = return [
        ("text/html", do liftIO (modifyIORef ref (+1))
                         count <- liftIO (readIORef ref)
                         let text = pack ("You are visitor number " ++ show count)
                         html text
        )
      ]
    }
