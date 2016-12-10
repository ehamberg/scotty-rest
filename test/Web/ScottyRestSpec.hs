{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.ScottyRestSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Internal
import Test.QuickCheck         (Arbitrary, arbitrary, elements, property)

import           Web.Scotty.Rest  (EndpointConfig(..), StdMethod (..))
import qualified Web.Scotty.Rest  as Rest
import           Web.Scotty.Trans hiding (delete, get, patch, post, put, request)

import Control.Monad.State     (evalStateT)
import Data.ByteString.Char8   (pack)
import Data.String.Conversions (cs)
import Data.Text.Lazy          (intercalate)

instance Arbitrary Rest.StdMethod where
  arbitrary = elements (enumFromTo minBound maxBound)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let withApp = with . scottyAppT id

  describe "Conditional requests" $ do
    describe "If-Match" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
                allowedMethods = return [DELETE]
              }) $
        it "makes sure we get a 412 Precondition Failed when e-tag does not match" $
          request "DELETE" "/" [("if-match", "")] "" `shouldRespondWith` 412

      withApp (Rest.rest "/" Rest.defaultConfig {
                allowedMethods = return [POST],
                resourceExists = return False
              }) $
        it "makes sure we get a 412 Precondition Failed for POST to non-existing" $ do
          request "POST" "/" [("if-match", "\"foo\", \"bar\"")] ""
            `shouldRespondWith` 412

    describe "If-None-Match" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
                allowedMethods = return [DELETE],
                generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get a 412 Precondition Failed for DELETE when e-tag matches" $ do
          let expectedHeaders = ["Etag" <:> "\"foo\""]
          request "DELETE" "/" [("if-none-match", "\"foo\"")] ""
            `shouldRespondWith` 412 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                allowedMethods = return [DELETE],
                generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get a 412 Precondition Failed for DELETE when e-tag matches" $ do
          let expectedHeaders = ["Etag" <:> "\"foo\""]
          request "DELETE" "/" [("if-none-match", "\"foo\", \"bar\"")] ""
            `shouldRespondWith` 412 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "foo")],
                generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we don't get a 304 Not Changed for GET when e-tag doesn't match" $ do
          let expectedHeaders = ["Etag" <:> "\"foo\""]
          request "GET" "/" [("if-none-match", "\"bar\"")] ""
            `shouldRespondWith` 200 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", undefined)],
                generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get a 304 Not Changed for GET when e-tag matches" $ do
          let expectedHeaders = ["Etag" <:> "\"foo\""]
          request "GET" "/" [("if-none-match", "\"foo\"")] ""
            `shouldRespondWith` 304 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", undefined)],
                generateEtag = return (Just (Rest.Strong "bar"))
              }) $
        it "makes sure we get a 304 Not Changed for GET when e-tag matches" $ do
          let expectedHeaders = ["Etag" <:> "\"bar\""]
          request "GET" "/" [("if-none-match", "\"foo\", \"bar\"")] ""
            `shouldRespondWith` 304 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "foo")],
                generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get a 304 Not Changed for if-none-match *" $ do
          let expectedHeaders = ["Etag" <:> "\"foo\""]
          request "GET" "/" [("if-none-match", "*")] ""
            `shouldRespondWith` 304 {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
                allowedMethods = return [DELETE]
              }) $
        it "makes sure we get a 412 Precondition Failed for if-none-match * for non-GET/HEAD" $ do
          request "DELETE" "/" [("if-none-match", "*")] ""
            `shouldRespondWith` 412

    describe "If-Unmodified-Since" $ do
      let time = read" 2015-01-01 12:00:00.000000 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", undefined)],
                lastModified = return (Just time)
              }) $
        it "makes sure we get a 412 Not Changed when not modified since given date" $ do
          let expectedHeaders = ["Last-Modified" <:> "Thu, 01 Jan 2015 12:00:00 GMT"]
          request "GET" "/" [("if-unmodified-since", "Thu, 31 May 2014 20:00:00 GMT")] ""
            `shouldRespondWith` 412 {matchHeaders = expectedHeaders}

    describe "If-Modified-Since" $ do
      let time = read" 2015-01-01 12:00:00.000000 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", undefined)],
                lastModified = return (Just time)
              }) $
        it "makes sure we get a 304 Not Changed when not modified since given date" $ do
          let expectedHeaders = ["Last-Modified" <:> "Thu, 01 Jan 2015 12:00:00 GMT"]
          request "GET" "/" [("if-modified-since", "Thu, 31 May 2015 20:00:00 GMT")] ""
            `shouldRespondWith` 304 {matchHeaders = expectedHeaders}


  describe "ETag/Expires/Last-Modified headers" $ do
    describe "ETag" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "hello")]
              , generateEtag = return (Just (Rest.Strong "foo"))
              }) $
        it "makes sure we get an ETag header" $
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = ["ETag" <:> "\"foo\""]}

      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "hello")]
              , generateEtag = return (Just (Rest.Weak "foo"))
              }) $
        it "makes sure we get a (weak) ETag header" $
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = ["ETag" <:> "W/\"foo\""]}

    describe "Expires" $ do
      let now = read "2015-02-16 13:11:11.753542 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "hello")]
              , expires = return (Just now)
              }) $
        it "makes sure we get an Expires header" $ do
          let expectedHeaders = ["Expires" <:> (cs . Rest.toHttpDateHeader) now]
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = expectedHeaders}

    describe "Last-Modified" $ do
      let time = read" 2015-01-01 12:00:00.000000 UTC"
      withApp (Rest.rest "/" Rest.defaultConfig {
                contentTypesProvided = return [("text/html", text "hello")]
              , lastModified = return (Just time)
              }) $
        it "makes sure we get a Last-Modified header" $ do
          let expectedHeaders = ["Last-Modified" <:> (cs . Rest.toHttpDateHeader) time]
          request "GET" "/" [] "" `shouldRespondWith` "hello" {matchHeaders = expectedHeaders}

  describe "HTTP (OPTIONS)" $ do
    describe "Allow header" $ do
      withApp (Rest.rest "/" Rest.defaultConfig) $
        it "makes sure we get a list of allowed methods for OPTIONS" $ do
          let expectedHeaders = ["Allow" <:> "GET, HEAD, OPTIONS"]
          request "OPTIONS" "/" [] "" `shouldRespondWith` "" {matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {allowedMethods = return [GET, OPTIONS, POST, PATCH]}) $
        it "makes sure we get a list of allowed methods for OPTIONS" $ do
          let expectedHeaders = ["Allow" <:> "GET, OPTIONS, POST, PATCH"]
          request "OPTIONS" "/" [] "" `shouldRespondWith` "" {matchHeaders = expectedHeaders}

    describe "Custom OPTIONS handler" $
      withApp (Rest.rest "/" Rest.defaultConfig {optionsHandler = return (Just ("text/plain", text "xyz"))}) $
        it "makes sure a custom OPTIONS handler is run" $
          request "OPTIONS" "/" [] ""
            `shouldRespondWith` "xyz" {matchHeaders = ["Content-Type" <:> "text/plain"]}

  describe "HTTP (DELETE)" $ do
    describe "DELETE existing: Fail" $
      withApp (Rest.rest "/" Rest.defaultConfig {allowedMethods = return [DELETE]}) $
        it "makes sure we get a 500 if deleteResource fails" $
          request "DELETE" "/" [] "" `shouldRespondWith` 500

    describe "DELETE existing: Enacted" $
      withApp (Rest.rest "/" Rest.defaultConfig {
                deleteResource = return Rest.DeleteEnacted
              , allowedMethods = return [DELETE]
              }) $
        it "makes sure we get a 202 if delete is enacted" $
          request "DELETE" "/" [] "" `shouldRespondWith` 202

    describe "DELETE existing: Completed" $
      withApp (Rest.rest "/" Rest.defaultConfig {
                deleteResource = return Rest.Deleted
              , allowedMethods = return [DELETE]
              }) $
        it "makes sure we get a 204 if delete is completed" $
          request "DELETE" "/" [] "" `shouldRespondWith` 204

  describe "HTTP (POST)" $ do
    describe "200: Created with content" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [
            ("text/plain", return (Rest.SucceededWithContent "text/html" "hi"))
          ]
        }) $
        it "makes sure we get a 200 when handler returns SucceededWithContent" $ do
          let expectedHeaders = ["Content-Type" <:> "text/html"]
          request "POST" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "hi" {matchStatus = 200, matchHeaders = expectedHeaders}

    describe "201: Created" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          resourceExists = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [
            ("text/plain", return (Rest.SucceededWithLocation "foo.bar"))
          ]
        }) $
        it "makes sure we get a 201 when handler returns SucceededWithLocation" $ do
          let expectedHeaders = ["Location" <:> "foo.bar"]
          request "POST" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 201, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PUT],
          resourceExists = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [
            ("text/plain", return Rest.Succeeded)
          ]
        }) $
        it "makes sure we get a 201 on PUT to non-existing when handler returns Succeeded" $ do
          request "PUT" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 201}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          resourceExists = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [
            ("text/plain", return (Rest.SucceededWithContent "text/plain" "foo"))
          ]
        }) $
        it "makes sure we get a 201 when handler returns SucceededWithLocation" $ do
          let expectedHeaders = ["Content-Type" <:> "text/plain"]
          request "POST" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "foo" {matchStatus = 201, matchHeaders = expectedHeaders}

    describe "204: No Content" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("text/plain", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 204 when handler returns Succeeded" $
          request "POST" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 204}

    describe "204: No Content" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PUT],
          resourceExists = return True,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("text/plain", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 204 when handler returns Succeeded" $
          request "PUT" "/" [("Content-Type", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 204}

    describe "204: POSTing to missing resource that existed previously" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 204 when POSTing to resource that existed, when allowing POSTing to missing resource" $
          request "POST" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` "" {matchStatus = 204}

    describe "404: POSTint to never-existed" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          resourceExists = return False,
          previouslyExisted = return False,
          allowMissingPost = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 404 when POSTing to a never-existed resource when not allowing missing POSTs" $
          request "POST" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` "" {matchStatus = 404}

  describe "HTTP (PATCH)" $
    describe "404: PATCHing a never-existed" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PATCH],
          resourceExists = return False,
          previouslyExisted = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 404 when PATCHing a never-existed resource" $
          request "PATCH" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` "" {matchStatus = 404}

  describe "HTTP (Vary)" $ do
    describe "Multiple content types provided" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "foo"), ("text/html", undefined)]
        }) $
        it "makes sure we get a Vary header with `Accept` when offering several content types" $ do
          let expectedHeaders = ["Vary" <:> "Accept"]
          request "GET" "/" [] "" `shouldRespondWith` "foo" {matchStatus = 200, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "foo"), ("text/html", undefined)]
        , languagesProvided = return (Just ["en", "fr"])
        }) $
        it "makes sure the Vary header also includes `Accept-Langauge` when offering several languages" $ do
          let expectedHeaders = ["Vary" <:> "Accept-Language, Accept"]
          request "GET" "/" [] "" `shouldRespondWith` "foo" {matchStatus = 200, matchHeaders = expectedHeaders}

    describe "Multiple languages provided" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "foo")],
          languagesProvided = return (Just ["en-gb", "de"])
        }) $
        it "makes sure we get a Vary header with `Accept` when offering several languages" $ do
          let expectedHeaders = ["Vary" <:> "Accept-Language"]
          request "GET" "/" [] "" `shouldRespondWith` "foo" {matchStatus = 200, matchHeaders = expectedHeaders}

    describe "Multiple charsets provided" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "foo")],
          charsetsProvided = return (Just ["ascii", "utf-8"])
        }) $
        it "makes sure we get a Vary header with `Accept` when offering several languages" $ do
          let expectedHeaders = ["Vary" <:> "Accept-Charset"]
          request "GET" "/" [] "" `shouldRespondWith` "foo" {matchStatus = 200, matchHeaders = expectedHeaders}

  describe "HTTP (General)" $ do
    describe "Authorized" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/html", text "xxx")],
          isAuthorized = return Rest.Authorized
        }) $
        it "makes sure we get a resource when we are authorized" $ do
          request "GET" "/" [] ""
            `shouldRespondWith` "xxx" {matchStatus = 200}

    describe "300: Multiple Representations" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/html", text "xxx")],
          multipleChoices = return (Rest.MultipleRepresentations "text/plain" "foo")
        }) $
        it "makes sure we get a 300 with a body when there are multiple representations" $ do
          let expectedHeaders = ["Content-Type" <:> "text/plain"]
          request "GET" "/" [] ""
            `shouldRespondWith` "foo" {matchStatus = 300, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/html", text "xxx")],
          multipleChoices = return (Rest.MultipleRepresentations "text/plain" "foo")
        }) $
        it "makes sure we get a 300 without a body for HEAD when there are multiple representations" $ do
          let expectedHeaders = ["Content-Type" <:> "text/plain"]
          request "HEAD" "/" [] ""
            `shouldRespondWith` "" {matchStatus = 300, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PUT],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return (Rest.SucceededWithContent "text/plain" "xxx"))],
          multipleChoices = return (Rest.MultipleRepresentations "text/plain" "foo")
        }) $
        it "makes sure we get a 300 with a body when there are multiple representations" $ do
          let expectedHeaders = ["Content-Type" <:> "text/plain"]
          request "PUT" "/" [("Content-Type", "application/json")] ""
            `shouldRespondWith` "foo" {matchStatus = 300, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return (Rest.SucceededWithContent "text/plain" "xxx"))],
          multipleChoices = return (Rest.MultipleWithPreferred "text/plain" "foo" "foo.bar")
        }) $
        it "makes sure we get a 300 with a location body when there are multiple representations, where one is preferred" $ do
          let expectedHeaders = ["Location" <:> "foo.bar", "Content-Type" <:> "text/plain"]
          request "POST" "/" [("Content-Type", "application/json")] ""
            `shouldRespondWith` "foo" {matchStatus = 300, matchHeaders = expectedHeaders}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [GET],
          contentTypesProvided = return [("text/html", undefined)],
          multipleChoices = return (Rest.MultipleWithPreferred "text/plain" "foo" "foo.bar")
        }) $
        it "makes sure we get a 300 with a location body when there are multiple representations, where one is preferred" $ do
          let expectedHeaders = ["Location" <:> "foo.bar", "Content-Type" <:> "text/plain"]
          request "GET" "/" [] "" `shouldRespondWith` "foo" {matchStatus = 300, matchHeaders = expectedHeaders}

    describe "301 Moved Permanently" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          resourceMoved = return (Rest.MovedPermanently "xxx"),
          contentTypesProvided = return [("text/html", undefined)]
        }) $
        it "makes sure we get a 301 when a resource existed before and is moved permanently" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 301, matchHeaders = ["Location" <:> "xxx"]}

    describe "307 Moved Temporarily" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          resourceMoved = return (Rest.MovedTemporarily "xxx"),
          contentTypesProvided = return [("text/html", undefined)]
        }) $
        it "makes sure we get a 307 when a resource existed before and is moved temporarily" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 307, matchHeaders = ["Location" <:> "xxx"]}

    describe "405 Method not allowed" $
      it "makes sure we get a 405 and an Allow header when method is not allowed" $
        property $ \m ms -> do
          app <- scottyAppT id (Rest.rest "/" Rest.defaultConfig {allowedMethods = return ms})
          let known = [GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS]
          let check = if | m `notElem` known -> (`shouldRespondWith` 501)
                         | m `notElem` ms    -> (`shouldRespondWith` 405 {matchHeaders = ["allow" <:> (cs . intercalate ", " . map (cs . show)) ms]})
                         | otherwise         -> \_ -> return ()
          let waiSession = check (request ((pack . show) m) "/" [] "")
          runWaiSession waiSession app

    describe "400 Bad Request" $
      withApp (Rest.rest "/" Rest.defaultConfig {malformedRequest = return True}) $
        it "makes sure we get a 400 when request is considered malformed" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 400}

    describe "401 Unauthorized" $
      withApp (Rest.rest "/" Rest.defaultConfig {isAuthorized = return (Rest.NotAuthorized "Basic")}) $
        it "makes sure we get a 401 when access is not authorized" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 401, matchHeaders = ["WWW-Authenticate" <:> "Basic"]}

    describe "403 Forbidden" $
      withApp (Rest.rest "/" Rest.defaultConfig {forbidden = return True}) $
        it "makes sure we get a 403 when access is forbidden" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 403}

    describe "404 Not Found" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          contentTypesProvided = return [("text/html", undefined)]
        }) $
        it "makes sure we get a 404 when resource does not exist and did not exist previously" $
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 404}

    describe "406 Not Acceptable" $ do
      withApp (Rest.rest "/" Rest.defaultConfig) $
        it "makes sure we get a 406 when we don't provided any types" $
          request "GET" "/" [("Accept", "text/html")] ""
            `shouldRespondWith` "" {matchStatus = 406}

      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html", text "")]
      }) $
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept", "text/html; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 406}

      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html; charset=utf-8", text "")]
      }) $
        it "makes sure we get a 406 when we don't provided the requested type" $ do
          request "GET" "/" [("Accept", "*/*")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept", "text/html; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept", "text/html; charset=latin1")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept", "text/*; charset=utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}

      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html", text "")],
        languagesProvided = return (Just ["en-gb", "de"])
      }) $
        it "makes sure we get a 406 when we don't provided the requested language" $ do
          request "GET" "/" [] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Language", "en-gb")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Language", "en-GB")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Language", "en")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Language", "en-US")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept-Language", "de")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Language", "de-DE")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept-Language", "no")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept-Language", "no-NB")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept-Language", "*")] ""
            `shouldRespondWith` "" {matchStatus = 200}

      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html", text "")],
        charsetsProvided = return (Just ["utf-8"])
      }) $
        it "makes sure we get a 406 when we don't provided the requested charset" $ do
          request "GET" "/" [] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Charset", "UTF-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Charset", "utf-8")] ""
            `shouldRespondWith` "" {matchStatus = 200}
          request "GET" "/" [("Accept-Charset", "ISO-8859-15")] ""
            `shouldRespondWith` "" {matchStatus = 406}

    describe "409 Conflict" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [PUT, PATCH],
          isConflict = return True,
          contentTypesProvided = return [("text/plain", undefined)],
          contentTypesAccepted = return [("text/plain", undefined)]
        }) $
        it "makes sure we get a 409 when there is a conflict" $ do
          request "PUT" "/" [("Content-Type", "text/plain"), ("Accept", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 409}
          request "PATCH" "/" [("Content-Type", "text/plain"), ("Accept", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 409}

    describe "410 Gone" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          resourceExists = return False,
          previouslyExisted = return True,
          allowedMethods = return [GET, POST, DELETE],
          allowMissingPost = return False,
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", undefined)]
        }) $
        it "makes sure we get a 410 when resource does not exist, but did exist previously" $ do
          request "GET" "/" [] "" `shouldRespondWith` "" {matchStatus = 410}
          request "POST" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` "" {matchStatus = 410}
          request "DELETE" "/" [] "" `shouldRespondWith` "" {matchStatus = 410}

    describe "415: Unsupported Media Type" $ do
      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST]
        }) $
        it "makes sure we get a 415 when POSTing to server that accepts nothing" $
          request "POST" "/" [("Content-Type", "application/json")] "" `shouldRespondWith` "" {matchStatus = 415}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("application/json", return Rest.Succeeded)]
        }) $
        it "makes sure we get a 415 when POSTing with invalid content-type" $
          request "POST" "/" [("Content-Type", "--")] "" `shouldRespondWith` "" {matchStatus = 415}

      withApp (Rest.rest "/" Rest.defaultConfig {
          allowedMethods = return [POST],
          contentTypesProvided = return [("text/html", undefined)],
          contentTypesAccepted = return [("text/plain", undefined)]
        }) $
        it "makes sure we get a 415 when POSTing with no content-type header" $ do
          request "POST" "/" [] ""
            `shouldRespondWith` 415


    describe "500 Internal Server Error" $
      withApp (Rest.rest "/" Rest.defaultConfig {serviceAvailable = raise $ stringError "XXX"}) $
        it "makes sure we get a 500 when throwing a string error" $
          request "GET" "/" [] "" `shouldRespondWith` 500

    describe "501 Not Implemented" $
      withApp (Rest.rest "/" Rest.defaultConfig) $
        it "makes sure we get a 501 when using an unimplemented method" $
          request "CONNECT" "/" [] "" `shouldRespondWith` 501

    describe "503 Not available" $
      withApp (Rest.rest "/" Rest.defaultConfig {serviceAvailable = return False}) $
        it "makes sure we get a 503 when serviceAvailable returns False" $
          request "GET" "/" [] "" `shouldRespondWith` 503

    describe "Content negotiation" $
      withApp (Rest.rest "/" Rest.defaultConfig {
        contentTypesProvided = return [("text/html", text "html"), ("application/json", json ("json" :: String))]
      }) $
        it "makes sure we get the appropriate content" $ do
          request "GET" "/" [] ""
            `shouldRespondWith` "html" {matchStatus = 200}
          request "GET" "/" [("Accept", "*/*")] ""
            `shouldRespondWith` "html" {matchStatus = 200}
          request "GET" "/" [("Accept", "application/*")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept", "application/json")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept", "text/plain")] ""
            `shouldRespondWith` "" {matchStatus = 406}
          request "GET" "/" [("Accept", "text/html;q=0.5, application/json")] ""
            `shouldRespondWith` "\"json\"" {matchStatus = 200}
          request "GET" "/" [("Accept", "text/html;q=0.5, application/json;q=0.4")] ""
            `shouldRespondWith` "html" {matchStatus = 200}

  describe "Exceptions" $
    describe "Recovering from exceptions" $
      withApp (Rest.rest "/" Rest.defaultConfig {
              contentTypesProvided = return [("text/html", raise (stringError "XXX") `rescue` (text . showError))]
        }) $
        it "makes sure we can recover from a `raise`" $
          request "GET" "/" [] ""
            `shouldRespondWith` "Internal server error: XXX" {matchStatus = 200}

  describe "Test servers" $
    describe "Echo server" $
      withApp (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "wtf")],
          contentTypesAccepted = return [("text/plain", fmap (Rest.SucceededWithContent  "text/plain" . cs) body)],
          allowedMethods = return [POST]
        }) $
        it "makes sure we can POST a text/plain body and get it back" $
          request "POST" "/" [("Content-Type", "text/plain"), ("Accept", "text/plain")] "hello"
            `shouldRespondWith` "hello" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain"]}

  describe "Custom monad" $
    describe "Server using a custom monad" $
      (with . scottyAppT (`evalStateT` Nothing)) (Rest.rest "/" Rest.defaultConfig {
          contentTypesProvided = return [("text/plain", text "wtf")],
          contentTypesAccepted = return [("text/plain", fmap (Rest.SucceededWithContent  "text/plain" . cs) body)],
          allowedMethods = return [POST]
        }) $
        it "makes sure we can POST a text/plain body and get it back" $
          request "POST" "/" [("Content-Type", "text/plain"), ("Accept", "text/plain")] "hello"
            `shouldRespondWith` "hello" {matchStatus = 200, matchHeaders = ["Content-Type" <:> "text/plain"]}
