{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Exception                    (SomeException, try)
import Control.Monad.State
import Data.Aeson
import Data.Foldable
import Data.Text.Lazy                       hiding (map)
import Database.SQLite.Simple               hiding (fold)
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import Web.Scotty.Rest
import Web.Scotty.Trans                     hiding (get, put)


data Message = Message {
  message :: Text
} deriving Generic

instance FromJSON Message

app :: IO ()
app = scottyT 3000 (`evalStateT` Nothing) $ do
  middleware logStdoutDev
  rest "/messages" defaultConfig { allowedMethods       = allowed
                                 , serviceAvailable     = available
                                 , contentTypesProvided = provided
                                 , contentTypesAccepted = accepted
                                 }
  where
    available = do
      -- Try to open the database, if we fail, the service is not available, so
      -- we return False.
      connection <- liftIO (try (open "test.db") :: IO (Either SomeException Connection))
      case connection of
           Left ex    -> (html . pack . show) ex >> return False
           Right conn -> do
             liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS messages (message varchar)"
             lift (put (Just conn)) -- store the database connection handle in the state
             return True
    allowed = return [GET, HEAD, POST, OPTIONS]
    provided = return [
        ("text/html", do
          Just conn <- lift get
          messages <- liftIO $ query_ conn "SELECT message FROM messages LIMIT 10"
          text $ fold $ map (\(Only m) -> pack m `snoc` '\n') messages
        ),
        ("application/json", do
          Just conn <- lift get
          messages <- liftIO $ query_ conn "SELECT message FROM messages LIMIT 10"
          text $ fold $ map (\(Only m) -> pack m `snoc` '\n') messages
        )
      ]
    accepted = return [
        ("application/json", do
          Just conn <- lift get
          Message msg <- jsonData
          liftIO $ execute conn "INSERT INTO messages(message) VALUES (?)" (Only msg)
          return Succeeded
        )
      ]

main :: IO ()
main = app
