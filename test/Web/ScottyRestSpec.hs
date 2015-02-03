{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.ScottyRestSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal
import Test.QuickCheck (Arbitrary, arbitrary, elements, property)

import Web.Scotty.Trans hiding (get, post, put, patch, delete, request)
import qualified Web.Scotty.Rest as Rest
import Web.Scotty.Rest (RestConfig(..), StdMethod(..))

import Data.ByteString.Char8 (pack)
import Network.Wai (Application)

instance Arbitrary Rest.StdMethod where
  arbitrary = elements (enumFromTo minBound maxBound)

main :: IO ()
main = hspec spec

withApp :: ScottyT e IO () -> SpecWith Application -> Spec
withApp = with . scottyAppT id id

spec :: Spec
spec = do
  describe "HTTP" $ do
    describe "503 Not available" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {serviceAvailable = return False}) $ do
        it "makes sure we get a 503 when serviceAvailable returns False" $ do
          request "GET" "/" [] "" `shouldRespondWith` 503

    describe "405 Method not allowed" $ do
      it "makes sure we get a 405 when method is not allowed" $ do
        property $ \m ms -> do
          app <- scottyAppT id id (Rest.rest "/" Rest.defaultConfig {allowedMethods = return ms})
          let expected = if | m == OPTIONS && m `elem` ms -> 200
                            | m `elem` ms                 -> 406
                            | m `notElem` ms              -> 405
          let waiSession = request ((pack . show) m) "/" [] "" `shouldRespondWith` "" {matchStatus = expected}
          runWaiSession waiSession app

    describe "401 Unauthorized" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {isAuthorized = return (Rest.NotAuthorized "Basic")}) $ do
        it "makes sure we get a 401 when access is not authorized" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 401, matchHeaders = ["WWW-Authenticate" <:> "Basic"]}

    describe "406 Not Acceptable" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html",text "")]
      }) $ do
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept","*/*")]        ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept","text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 406}

    describe "Content negotiation" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html",text "html"), ("application/json",json ("json" :: String))]
      }) $ do
        it "makes sure we get the appropriate content" $ do
          request "GET" "/" [] ""
            `shouldRespondWith` "html" {matchStatus = 200}
          request "GET" "/" [("Accept","*/*")] ""
            `shouldRespondWith` "html" {matchStatus = 200}
          request "GET" "/" [("Accept","application/*")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept","application/json")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept","text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept","text/html;q=0.5, application/json")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept","text/html;q=0.5, application/json;q=0.4")] ""
            `shouldRespondWith` "html" {matchStatus = 200}
