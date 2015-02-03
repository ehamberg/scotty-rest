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

    describe "404 Not Found" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          contentTypesProvided = return [("text/html",undefined)]
        }) $ do
        it "makes sure we get a 404 when resource does not exist and did not exist previously" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 404}

    describe "410 Gone" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          allowedMethods = return [GET, POST],
          contentTypesProvided = return [("text/html",undefined)],
          contentTypesAccepted = return [("application/json",undefined)]
        }) $ do
        it "makes sure we get a 410 when resource does not exist, but did exist previously" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 410}
          -- request "POST" "/" [("Content-Type","application/json")] "" `shouldRespondWith` "" {matchStatus = 410}

    describe "301 Moved Permanently" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          movedPermanently = return (Rest.MovedTo "xxx"),
          contentTypesProvided = return [("text/html",undefined)]
        }) $ do
        it "makes sure we get a 301 when a resource existed before and is moved permanently" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 301, matchHeaders = ["Location" <:> "xxx"]}

    describe "307 Moved Temporarily" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          movedTemporarily = return (Rest.MovedTo "xxx"),
          contentTypesProvided = return [("text/html",undefined)]
        }) $ do
        it "makes sure we get a 307 when a resource existed before and is moved temporarily" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 307, matchHeaders = ["Location" <:> "xxx"]}

    describe "406 Not Acceptable" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html",text "")]
      }) $ do
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept","*/*")] ""
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
