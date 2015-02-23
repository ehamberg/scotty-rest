{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Rest
  (
  -- * REST handler to Scotty
    rest
  -- * Callback result types
  , Authorized(..)
  , DeleteResult(..)
  , ETag(..)
  , Moved(..)
  , ProcessingResult(..)
  -- * Config
  , RestConfig(..)
  , defaultConfig
  -- * Re-exports
  , MediaType
  , StdMethod(..)
  , UTCTime
  -- * Utilities
  , toHttpDateHeader
  ) where

import BasePrelude hiding (Handler)

import Web.Scotty.Rest.Types

import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy      as BS
import           Data.Convertible          (convert)
import           Data.Default.Class        (Default (..), def)
import           Data.String.Conversions   (convertString)
import qualified Data.Text.Lazy            as TL
import           Data.Time.Calendar        (fromGregorian)
import           Data.Time.Clock           (UTCTime (..), secondsToDiffTime)
import           Network.HTTP.Date
import           Network.HTTP.Media        (mapAccept, mapContent, matches, parseAccept,
                                            renderHeader)
import           Network.HTTP.Types        (parseMethod)
import           Network.HTTP.Types.Status
import qualified Network.Wai               as Wai
import           Web.Scotty.Trans

defaultConfig :: RestConfig
defaultConfig = def

-- | /rest/ is used where you would use e.g. 'get' in your Scotty app, and
-- will match any method:
--
-- > main = scotty 3000 $ do
-- >   get  "/foo" (text "Hello!")
-- >   rest "/bar" defaultConfig {
-- >       contentTypesProvided = return [("text/html",html "Hello, World!")]
-- >     }
rest :: RoutePattern -> RestConfig -> ScottyT RestException IO ()
rest pattern config = matchAny pattern $ do
  initialState <- emptyHanderState config
  let run = runReaderT (runRestM restHandlerStart) initialState
  run `rescue` handleExcept

handler :: RestM (Handler ())
handler = computeOnce handler' $ do
  -- If there is an `Accept` header -- look at the content types we provide and
  -- find and store the best handler. If we cannot provide that type, stop
  -- processing here and return a NotAcceptable406:
  accept <- return . convertString . fromMaybe "*/*" =<< header' "accept"
  provided <- contentTypesProvided =<< retrieve config'
  maybe (stopWith NotAcceptable406) return (mapAccept provided accept)

language :: RestM (Maybe Language)
language = computeOnce language' (findPreferred "accept-language" parse languagesProvided match)
  where parse = parseAccept . convertString
        match = flip matches

charset :: RestM (Maybe TL.Text)
charset = computeOnce charset' (findPreferred "accept-charset" parse charsetsProvided match)
  where parse     = Just
        match a b = TL.toCaseFold a == TL.toCaseFold b

findPreferred :: TL.Text -> (TL.Text -> Maybe a) -> (RestConfig -> RestM (Maybe [a])) -> (a -> a -> Bool) -> RestM (Maybe a)
findPreferred headerName parse provided match = do
  -- If there is an `Accept-{Charsets,Languages}` header and
  -- `{Charsets,Languages}Provided` is defined, look at what we provide and
  -- find and store the first acceptable one (languages and charsets are in
  -- order of preference). If we cannot provide the requested one, stop
  -- processing here and return a NotAcceptable406.
  headerAndConfig <- runMaybeT $ do
      accept  <- MaybeT (header' headerName)
      provide <- MaybeT (provided =<< retrieve config')
      return (accept,provide)
  case headerAndConfig of
       Nothing    -> return Nothing
       Just (a,p) -> do
         -- We now have a new failure mode: Since there is a header, and a list
         -- of languages/charsets, failing to parse the header, or failing to
         -- find a match, will now lead to a 406 Not Acceptable:
         best <- runMaybeT $ do
           requested <- MaybeT ((return . parse) a)
           MaybeT ((return . head' . filter (match requested)) p)
         when (isNothing best) (stopWith NotAcceptable406)
         return best
  where head' [] = Nothing
        head' (x:_) = Just x

requestMethod :: RestM StdMethod
requestMethod = computeOnce method' $ do
  req <- (RestM . lift) request
  case (parseMethod .Wai.requestMethod) req of
       Left  _       -> stopWith NotImplemented501
       Right method  -> if method `elem` [GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS]
                           then return method
                           else stopWith NotImplemented501

eTag :: RestM (Maybe ETag)
eTag = computeOnce eTag' (fromConfig generateEtag)

isAvailable :: RestM Bool
isAvailable = computeOnce isAvailable' (fromConfig serviceAvailable)

modificationDate :: RestM (Maybe UTCTime)
modificationDate = computeOnce lastModified' (fromConfig lastModified)

expiryTime :: RestM (Maybe UTCTime)
expiryTime = computeOnce expires' (fromConfig expires)

restHandlerStart :: RestM ()
restHandlerStart = do
  -- Is our service available?
  available <- isAvailable
  unless available (stopWith ServiceUnavailable503)

  -- Is the method known?
  method <- requestMethod

  -- TODO: Is the URI too long?

  -- Is the method allowed?
  allowed <- fromConfig allowedMethods
  when (method `notElem` allowed) $ do
    setAllowHeader
    stopWith MethodNotAllowed405

  -- TODO: Is the request malformed?

  -- Is the client authorized?
  fromConfig isAuthorized >>= \case
       Authorized                -> return ()
       (NotAuthorized challenge) -> setHeader' "WWW-Authenticate" challenge >> stopWith Unauthorized401

  -- TODO: Is the client forbidden to access this resource?
  -- TODO: Are the content headers valid?
  -- TODO: Is the entity length valid?

  if method == OPTIONS
     then handleOptions
     else contentNegotiationStart

setAllowHeader :: RestM ()
setAllowHeader = do
  allowed <- fromConfig allowedMethods
  setHeader' "allow" . TL.intercalate ", " . map (convertString . show) $ allowed

----------------------------------------------------------------------------------------------------
-- OPTIONS
----------------------------------------------------------------------------------------------------

handleOptions :: RestM ()
handleOptions = maybe setAllowHeader runHandler =<< fromConfig optionsHandler

----------------------------------------------------------------------------------------------------
-- Content negotiation
----------------------------------------------------------------------------------------------------

contentNegotiationStart :: RestM ()
contentNegotiationStart = contentNegotiationAccept

contentNegotiationAccept :: RestM ()
contentNegotiationAccept = do
  accept <- header' "accept"
  when (isJust accept) (void handler) -- evalute `handler` to force early 406 (Not acceptable)
  contentNegotiationAcceptLanguage

-- TODO: If there is an `Accept-Language` header, check that we provide that
-- language. If not → 406.
contentNegotiationAcceptLanguage :: RestM ()
contentNegotiationAcceptLanguage = do
  acceptLanguage <- header' "accept-language"
  when (isJust acceptLanguage) (void language) -- evalute `language` to force early 406 (Not acceptable)
  contentNegotiationAcceptCharSet

-- TODO: If there is an `Accept-Charset` header, check that we provide that
-- char set. If not → 406.
contentNegotiationAcceptCharSet :: RestM ()
contentNegotiationAcceptCharSet = do
  acceptCharset <- header' "accept-charset"
  when (isJust acceptCharset) (void charset) -- evalute `charset` to force early 406 (Not acceptable)
  contentNegotiationVariances

-- TODO
contentNegotiationVariances :: RestM ()
contentNegotiationVariances = checkResourceExists

checkResourceExists :: RestM ()
checkResourceExists = do
  method <- requestMethod
  exists <- fromConfig resourceExists
  if | method `elem` [GET, HEAD]        -> if exists
                                              then handleGetHeadExisting
                                              else handleGetHeadNonExisting
     | method `elem` [PUT, POST, PATCH] -> if exists
                                              then handlePutPostPatchExisting
                                              else handlePutPostPatchNonExisting
     | method `elem` [DELETE]           -> if exists
                                              then handleDeleteExisting
                                              else handleDeleteNonExisting

----------------------------------------------------------------------------------------------------
-- GET/HEAD
----------------------------------------------------------------------------------------------------

handleGetHeadExisting :: RestM ()
handleGetHeadExisting = do
  addEtagHeader
  addLastModifiedHeader
  addExpiresHeader
  runHandler =<< handler
  -- TODO: multiple choices

handleGetHeadNonExisting :: RestM ()
handleGetHeadNonExisting = handleNonExisting

moved :: RestM ()
moved = do
  fromConfig movedPermanently >>= moved' MovedPermanently301
  fromConfig movedTemporarily >>= moved' MovedTemporarily307
    where moved' _ NotMoved      = return ()
          moved' e (MovedTo url) = setHeader' "location" url >> stopWith e

----------------------------------------------------------------------------------------------------
-- PUT/POST/PATCH
----------------------------------------------------------------------------------------------------

handlePutPostPatchNonExisting :: RestM ()
handlePutPostPatchNonExisting = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  liftM isJust (header' "if-match") >>= (`when` stopWith PreconditionFailed412)
  methodIs [POST, PATCH] ppppreviouslyExisted pppmethodIsPut

ppppreviouslyExisted :: RestM ()
ppppreviouslyExisted = do
  existed <- fromConfig previouslyExisted
  if existed
     then pppmovedPermanently
     else pppmethodIsPost

pppmovedPermanently :: RestM ()
pppmovedPermanently = do
  moved
  methodIs [POST] (allowsMissingPost acceptResource (stopWith Gone410)) pppmethodIsPut

pppmethodIsPost :: RestM ()
pppmethodIsPost = methodIs [POST] pppmethodIsPut (stopWith NotFound404)

pppmethodIsPut :: RestM ()
pppmethodIsPut = do
  method <- requestMethod
  when (method == PUT) $ do
    conflict <- fromConfig isConflict
    when conflict (stopWith Conflict409)

  acceptResource

handlePutPostPatchExisting :: RestM ()
handlePutPostPatchExisting = do
  cond
  pppmethodIsPut

----------------------------------------------------------------------------------------------------
-- POST/PUT/PATCH, part 2: content-types accepted
----------------------------------------------------------------------------------------------------

acceptResource :: RestM ()
acceptResource = do
  -- Is there a Content-Type header?
  contentTypeHeader <- header' "content-type"
  contentType <- maybe (stopWith UnsupportedMediaType415) (return . convertString) contentTypeHeader

  -- Do we have a handler for this content type? If so, run it. Alternatively, return 415.
  handlers <- fromConfig contentTypesAccepted
  result <- maybe (stopWith UnsupportedMediaType415) runHandler (mapContent handlers contentType)

  case result of
       Failed                    -> status' badRequest400
       Succeeded                 -> status' noContent204
       SucceededSeeOther url     -> resourceSeeOther url
       SucceededWithLocation url -> resourceWithLocation url
       SucceededWithContent t c  -> resourceWithContent t c

resourceSeeOther :: Url -> RestM ()
resourceSeeOther url = do
  newResource <- cached newResource'
  if newResource
     then status' created201
     else setHeader' "location" url >> status' seeOther303

resourceWithLocation :: Url -> RestM ()
resourceWithLocation url = do
  newResource <- cached newResource'
  if newResource
     then do setHeader' "location" url
             status' created201
     else status' noContent204

resourceWithContent :: MediaType -> TL.Text -> RestM ()
resourceWithContent t c = do
  setContentTypeHeader t
  (raw' . convertString) c
  multiple <- fromConfig multipleChoices
  if multiple
     then status' multipleChoices300
     else status' ok200

----------------------------------------------------------------------------------------------------
-- DELETE
----------------------------------------------------------------------------------------------------

handleDeleteExisting :: RestM ()
handleDeleteExisting = do
  cond
  result <- fromConfig deleteResource
  when (result == NotDeleted) (stopWith (InternalServerError "Could not delete resource"))
  case result of
       DeleteEnacted             -> status' accepted202
       DeleteCompleted           -> status' noContent204
       (DeletedWithResponse t c) -> resourceWithContent t c
       NotDeleted                -> stopWith (InternalServerError "Deleting existing resource failed")

handleDeleteNonExisting :: RestM ()
handleDeleteNonExisting = handleNonExisting

----------------------------------------------------------------------------------------------------
-- Conditional requests
----------------------------------------------------------------------------------------------------

cond :: RestM ()
cond = condIfMatch

condIfMatch :: RestM ()
condIfMatch = header' "if-match" >>= \case
  Nothing  -> condIfUnmodifiedSince
  Just hdr -> eTagMatches hdr condIfUnmodifiedSince (stopWith PreconditionFailed412)

condIfUnmodifiedSince :: RestM ()
condIfUnmodifiedSince = checkModificationHeader "if-unmodified-since" >>= \case
       Nothing    -> condIfNoneMatch -- If there are any errors: continue
       Just False -> condIfNoneMatch
       Just True  -> stopWith PreconditionFailed412

condIfNoneMatch :: RestM ()
condIfNoneMatch = header' "if-none-match" >>= \case
  Nothing  -> condIfModifiedSince
  Just hdr -> eTagMatches hdr condIfModifiedSince match
    where match = methodIs [GET, HEAD]
                   (addEtagHeader >> addExpiresHeader >> stopWith NotModified304)
                   (stopWith PreconditionFailed412)

condIfModifiedSince :: RestM ()
condIfModifiedSince = checkModificationHeader "if-modified-since" >>= \case
       Nothing    -> return ()
       Just False -> (addEtagHeader >> addExpiresHeader >> stopWith NotModified304)
       Just True  -> return ()

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

addEtagHeader :: RestM ()
addEtagHeader = eTag >>= \case
    Nothing         -> return ()
    Just (Weak t)   -> setHeader' "etag" ("W/\"" <> t <> "\"")
    Just (Strong t) -> setHeader' "etag" ("\"" <> t <> "\"")

addLastModifiedHeader :: RestM ()
addLastModifiedHeader = modificationDate >>= \case
    Nothing -> return ()
    Just t  -> setHeader' "last-modified" (toHttpDateHeader t)

addExpiresHeader :: RestM ()
addExpiresHeader = expiryTime >>= \case
    Nothing  -> return ()
    Just t   -> setHeader' "expires" (toHttpDateHeader t)

checkModificationHeader :: TL.Text -> RestM (Maybe Bool)
checkModificationHeader hdr = runMaybeT $ do
    modDate   <- MaybeT modificationDate
    hdrDate   <- MaybeT (header' hdr)
    unmodDate <- MaybeT (return (parseHeaderDate hdrDate))
    return (modDate > unmodDate)

handleNonExisting :: RestM ()
handleNonExisting = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  liftM isJust (header' "if-match") >>= (`when` stopWith PreconditionFailed412)

  -- Did this resource exist before?
  existed <- fromConfig previouslyExisted
  unless existed (stopWith NotFound404)

  moved
  stopWith Gone410

setContentTypeHeader :: MediaType -> RestM ()
setContentTypeHeader = setHeader' "content-type" . convertString . renderHeader

methodIs :: [StdMethod] -> RestM () -> RestM () -> RestM ()
methodIs ms onTrue onFalse = do
  method <- requestMethod
  if method `elem` ms
     then onTrue
     else onFalse

allowsMissingPost :: RestM () -> RestM () -> RestM ()
allowsMissingPost onTrue onFalse = do
  allowed <- fromConfig allowMissingPost
  if allowed
     then newResource' `store` True >> onTrue
     else onFalse

eTagMatches :: TL.Text -> RestM () -> RestM () -> RestM ()
eTagMatches given onTrue onFalse = do
  tag <- eTag
  case tag of
       Nothing -> onFalse
       Just e  -> if eTagMatch e given
                     then onTrue
                     else onFalse
    where eTagMatch :: ETag -> TL.Text -> Bool
          eTagMatch = undefined

parseHeaderDate :: TL.Text -> Maybe UTCTime
parseHeaderDate hdr = do
  headerDate <- (parseHTTPDate . convertString) hdr
  let year = (fromIntegral . hdYear) headerDate
      mon  = hdMonth headerDate
      day  = hdDay headerDate
      h    = hdHour headerDate
      m    = hdMinute headerDate
      s    = hdSecond headerDate
      date = fromGregorian year mon day
      time = secondsToDiffTime . fromIntegral $ h*60*60 + m*60 + s
  return $ UTCTime date time

toHttpDateHeader :: UTCTime -> TL.Text
toHttpDateHeader = convertString . formatHTTPDate . epochTimeToHTTPDate . convert

stopWith :: RestException -> RestM a
stopWith = RestM . lift . raise

runHandler :: Handler a -> RestM a
runHandler = RestM . lift

setHeader' :: TL.Text -> TL.Text -> RestM ()
setHeader' h v = RestM . lift $ setHeader h v

header' :: TL.Text -> RestM (Maybe TL.Text)
header' = RestM . lift . header

status' :: Status -> RestM ()
status' = RestM . lift . status

raw' :: BS.ByteString -> RestM ()
raw' = RestM . lift .raw


handleExcept :: RestException -> ActionT RestException IO ()
handleExcept MovedPermanently301     = status movedPermanently301
handleExcept NotModified304          = status notModified304
handleExcept MovedTemporarily307     = status temporaryRedirect307
handleExcept BadRequest400           = status badRequest400
handleExcept Unauthorized401         = status unauthorized401
handleExcept NotFound404             = status notFound404
handleExcept MethodNotAllowed405     = status methodNotAllowed405
handleExcept NotAcceptable406        = status notAcceptable406
handleExcept Conflict409             = status conflict409
handleExcept UnsupportedMediaType415 = status unsupportedMediaType415
handleExcept Gone410                 = status gone410
handleExcept PreconditionFailed412   = status preconditionFailed412
handleExcept ServiceUnavailable503   = status serviceUnavailable503
handleExcept NotImplemented501       = status notImplemented501
handleExcept (InternalServerError s) = text s >> status internalServerError500
