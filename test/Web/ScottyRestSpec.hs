{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.ScottyRestSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal
import           Test.QuickCheck         (Arbitrary, arbitrary, elements, property)

import           Web.Scotty.Rest         (RestConfig (..), StdMethod (..))
import qualified Web.Scotty.Rest         as Rest
import           Web.Scotty.Trans        hiding (delete, get, patch, post, put, request)

import           Control.Monad           (liftM)
import           Data.ByteString.Char8   (pack)
import           Data.String.Conversions (cs)

instance Arbitrary Rest.StdMethod where
  arbitrary = elements (enumFromTo minBound maxBound)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let withApp = with . scottyAppT id id

  describe "ETag/Expires/Last-Modified headers" $ do
    describe "ETag" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html",text "hello")]
              , generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get an ETag header" $
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = ["ETag" <:> "\"foo\""]}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html",text "hello")]
              , generateEtag = return (Just (Rest.Weak "foo"))
              }) $
        it "makes sure we get a (weak) ETag header" $
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = ["ETag" <:> "W/\"foo\""]}

    describe "Expires" $ do
      let now = read "2015-02-16 13:11:11.753542 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html",text "hello")]
              , expires = return (Just now)
              }) $
        it "makes sure we get an Expires header" $ do
          let expectedHeaders = ["Expires" <:> (cs . Rest.toHttpDateHeader) now]
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = expectedHeaders}

    describe "Last-Modified" $ do
      let time = read" 2015-01-01 12:00:00.000000 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html",text "hello")]
              , lastModified = return (Just time)
              }) $
        it "makes sure we get a Last-Modified header" $ do
          let expectedHeaders = ["Last-Modified" <:> (cs . Rest.toHttpDateHeader) time]
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = expectedHeaders}

  describe "HTTP" $ do
    describe "OPTIONS" $ do
      withApp (Rest.rest "/" Rest.defaultConfig) $
        it "makes sure we get a list of allowed methods for OPTIONS" $ do
          let expectedHeaders = ["Allow" <:> "GET, HEAD, OPTIONS"]
          request "OPTIONS" "/" [] "" `shouldRespondWith` "" {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {allowedMethods = return [GET, OPTIONS, POST, PATCH]}) $
        it "makes sure we get a list of allowed methods for OPTIONS" $ do
          let expectedHeaders = ["Allow" <:> "GET, OPTIONS, POST, PATCH"]
          request "OPTIONS" "/" [] "" `shouldRespondWith` "" {matchHeaders = expectedHeaders}

    describe "503 Not available" $
      withApp (Rest.rest "/" Rest.defaultConfig {serviceAvailable = return False}) $
        it "makes sure we get a 503 when serviceAvailable returns False" $
          request "GET" "/" [] "" `shouldRespondWith` 503

    describe "405 Method not allowed" $
      it "makes sure we get a 405 when method is not allowed" $
        property $ \m ms -> do
          app <- scottyAppT id id (Rest.rest "/" Rest.defaultConfig {allowedMethods = return ms})
          let expected = if | m == OPTIONS && m `elem` ms -> 200
                            | m `elem` ms                 -> 406
                            | m `notElem` ms              -> 405
          let waiSession = request ((pack . show) m) "/" [] "" `shouldRespondWith` "" {matchStatus = expected}
          runWaiSession waiSession app

    describe "401 Unauthorized" $
      withApp (Rest.rest "/" Rest.defaultConfig {isAuthorized = return (Rest.NotAuthorized "Basic")}) $
        it "makes sure we get a 401 when access is not authorized" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 401, matchHeaders = ["WWW-Authenticate" <:> "Basic"]}

    describe "404 Not Found" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          contentTypesProvided = return [("text/html",undefined)]
        }) $
        it "makes sure we get a 404 when resource does not exist and did not exist previously" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 404}

    describe "410 Gone" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          allowedMethods = return [GET, POST],
          contentTypesProvided = return [("text/html",undefined)],
          contentTypesAccepted = return [("application/json",undefined)]
        }) $
        it "makes sure we get a 410 when resource does not exist, but did exist previously" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 410}
          -- request "POST" "/" [("Content-Type","application/json")] "" `shouldRespondWith` "" {matchStatus = 410}

    describe "301 Moved Permanently" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          movedPermanently = return (Rest.MovedTo "xxx"),
          contentTypesProvided = return [("text/html",undefined)]
        }) $
        it "makes sure we get a 301 when a resource existed before and is moved permanently" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 301, matchHeaders = ["Location" <:> "xxx"]}

    describe "307 Moved Temporarily" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          movedTemporarily = return (Rest.MovedTo "xxx"),
          contentTypesProvided = return [("text/html",undefined)]
        }) $
        it "makes sure we get a 307 when a resource existed before and is moved temporarily" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 307, matchHeaders = ["Location" <:> "xxx"]}

    describe "406 Not Acceptable" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html",text "")]
      }) $
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept","text/html; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept","text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 406}

      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html; charset=utf-8",text "")]
      }) $
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept","*/*")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept","text/html; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept","text/html; charset=latin1")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept","text/*; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}

    describe "409 Conflict" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PUT],
          isConflict = return True,
          contentTypesProvided = return [("text/plain",undefined)],
          contentTypesAccepted = return [("text/plain",undefined)]
        }) $
        it "makes sure we get a 409 when there is a conflict" $
          request "PUT" "/" [("Content-Type","text/plain"), ("Accept","text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 409}

    describe "Content negotiation" $
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html",text "html"), ("application/json",json ("json" :: String))]
      }) $
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

  describe "Test servers" $
    describe "Echo server" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain",text "wtf")],
          contentTypesAccepted = return [("text/plain",liftM (Rest.SucceededWithContent  "text/plain" . cs) body)],
          allowedMethods = return [POST]
        }) $
        it "makes sure we can POST a text/plain body and get it back" $
          request "POST" "/" [("Content-Type","text/plain"), ("Accept","text/plain")] "hello"
            `shouldRespondWith` "hello" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain"]}

