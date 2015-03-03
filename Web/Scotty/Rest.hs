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
  , Representation(..)
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
import           Network.HTTP.Media        (mapAccept, mapContent, matchAccept, matches,
                                            parseAccept, renderHeader)
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

preferred :: RestM (MediaType,Handler ())
preferred = computeOnce handler' $ do
  -- If there is an `Accept` header -- look at the content types we provide and
  -- find and store the best handler together with the content type.  If we
  -- cannot provide that type, stop processing here and return a
  -- NotAcceptable406:
  accept <- return . convertString . fromMaybe "*/*" =<< header' "accept"
  provided <- contentTypesProvided =<< retrieve config'
  contentType <- maybe (stopWith NotAcceptable406) return (matchAccept (map fst provided) accept)
  bestHandler <- maybe (stopWith NotAcceptable406) return (mapAccept provided accept)
  return (contentType,bestHandler)

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
handleOptions = fromConfig optionsHandler >>= \case
  Nothing                      -> setAllowHeader
  (Just (contentType,handler)) -> runHandler handler >> setContentTypeHeader contentType

----------------------------------------------------------------------------------------------------
-- Content negotiation
----------------------------------------------------------------------------------------------------

contentNegotiationStart :: RestM ()
contentNegotiationStart = contentNegotiationAccept

contentNegotiationAccept :: RestM ()
contentNegotiationAccept = do
  accept <- header' "accept"
  when (isJust accept) (void preferred) -- evalute `preferred` to force early 406 (Not acceptable)
  contentNegotiationAcceptLanguage

-- If there is an `Accept-Language` header, check that we provide that
-- language. If not → 406.
contentNegotiationAcceptLanguage :: RestM ()
contentNegotiationAcceptLanguage = do
  acceptLanguage <- header' "accept-language"
  when (isJust acceptLanguage) (void language) -- evalute `language` to force early 406 (Not acceptable)
  contentNegotiationAcceptCharSet

-- If there is an `Accept-Charset` header, check that we provide that
-- char set. If not → 406.
contentNegotiationAcceptCharSet :: RestM ()
contentNegotiationAcceptCharSet = do
  acceptCharset <- header' "accept-charset"
  when (isJust acceptCharset) (void charset) -- evalute `charset` to force early 406 (Not acceptable)
  contentNegotiationVariances

-- If we provide more than one content type, add `Accept` to `Vary` header. If
-- we provide a set of languages and/or charsets, add `Accept-Language` and
-- `Accept-Charset`, respectively, to the `Vary` header too.
contentNegotiationVariances :: RestM ()
contentNegotiationVariances = do
  config <- retrieve config'
  ctp <- contentTypesProvided config
  lp  <- languagesProvided config
  cp  <- charsetsProvided config
  varyHeader <- fromConfig variances
  let varyHeader'   = if length ctp > 1 then "Accept":varyHeader           else varyHeader
  let varyHeader''  = if isJust lp      then "Accept-Language":varyHeader' else varyHeader'
  let varyHeader''' = if isJust cp      then "Accept-Charset":varyHeader'' else varyHeader''
  setHeader' "vary" . TL.intercalate ", " $ varyHeader'''
  checkResourceExists

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
  cond
  addEtagHeader
  addLastModifiedHeader
  addExpiresHeader
  method <- requestMethod
  (contentType,handler) <- preferred
  fromConfig multipleChoices >>= \case
    MultipleRepresentations t' c' -> writeContent t' c' >> status' multipleChoices300
    MultipleWithPreferred t' c' u -> writeContent t' c' >> setHeader' "location" u >>  status' multipleChoices300
    UniqueRepresentation          -> do when (method == GET) (runHandler handler)
                                        setContentTypeHeader contentType
                                        status' ok200

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
  ifMethodIs [POST, PATCH]
    ppppreviouslyExisted
    pppmethodIsPut

ppppreviouslyExisted :: RestM ()
ppppreviouslyExisted = do
  existed <- fromConfig previouslyExisted
  if existed
     then pppmovedPermanentlyOrTemporarily
     else pppmethodIsPost

pppmovedPermanentlyOrTemporarily :: RestM ()
pppmovedPermanentlyOrTemporarily = do
  moved
  ifMethodIs [POST]
    (allowsMissingPost acceptResource (stopWith Gone410))
    pppmethodIsPut

pppmethodIsPost :: RestM ()
pppmethodIsPost =
  ifMethodIs [POST]
    (allowsMissingPost pppmethodIsPut (stopWith NotFound404))
    (stopWith NotFound404)

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

  exists <- fromConfig resourceExists

  case (result, exists) of
       (Failed, _)                        -> status' badRequest400
       (Succeeded, True)                  -> status' noContent204
       (Succeeded, False)                 -> status' created201
       (SucceededWithContent t c, True)   -> resourceWithContent t c
       (SucceededWithContent t c, False)  -> writeContent t c >> status' created201
       (SucceededWithLocation url, True)  -> locationAndResponseCode url noContent204
       (SucceededWithLocation url, False) -> locationAndResponseCode url created201
       (Redirect url, _)                  -> locationAndResponseCode url seeOther303
    where locationAndResponseCode url response = setHeader' "location" url >> status' response

writeContent :: MediaType -> TL.Text -> RestM ()
writeContent t c = setContentTypeHeader t >> (raw' . convertString) c

resourceWithContent :: MediaType -> TL.Text -> RestM ()
resourceWithContent t c = fromConfig multipleChoices >>= \case
  UniqueRepresentation          -> setContentTypeHeader t >> (raw' . convertString) c >> status' ok200
  MultipleRepresentations t' c' -> writeContent t' c' >> status' multipleChoices300
  MultipleWithPreferred t' c' u -> writeContent t' c' >> setHeader' "location" u >>  status' multipleChoices300

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
  Just hdr -> ifEtagMatches hdr
                condIfUnmodifiedSince
                (addEtagHeader >> stopWith PreconditionFailed412)

condIfUnmodifiedSince :: RestM ()
condIfUnmodifiedSince = modifiedSinceHeaderDate "if-unmodified-since" >>= \case
       Nothing    -> condIfNoneMatch -- If there are any errors: continue
       Just False -> condIfNoneMatch
       Just True  -> addLastModifiedHeader >> stopWith PreconditionFailed412

condIfNoneMatch :: RestM ()
condIfNoneMatch = header' "if-none-match" >>= \case
  Nothing  -> condIfModifiedSince
  Just hdr -> ifEtagMatches hdr
                match
                condIfModifiedSince
    where match = ifMethodIs [GET, HEAD]
                   notModified
                   (addEtagHeader >> stopWith PreconditionFailed412)

condIfModifiedSince :: RestM ()
condIfModifiedSince = modifiedSinceHeaderDate "if-modified-since" >>= \case
       Nothing    -> return ()
       Just False -> notModified
       Just True  -> return ()

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

notModified :: RestM ()
notModified = do
  addEtagHeader
  addExpiresHeader
  stopWith NotModified304

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

modifiedSinceHeaderDate :: TL.Text -> RestM (Maybe Bool)
modifiedSinceHeaderDate hdr = runMaybeT $ do
    modDate    <- MaybeT modificationDate
    headerText <- MaybeT (header' hdr)
    headerDate <- MaybeT (return (parseHeaderDate headerText))
    return (modDate > headerDate)

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

ifMethodIs :: [StdMethod] -> RestM () -> RestM () -> RestM ()
ifMethodIs ms onTrue onFalse = do
  method <- requestMethod
  if method `elem` ms
     then onTrue
     else onFalse

allowsMissingPost :: RestM () -> RestM () -> RestM ()
allowsMissingPost onTrue onFalse = do
  allowed <- fromConfig allowMissingPost
  if allowed
     then onTrue
     else onFalse

ifEtagMatches :: TL.Text -> RestM () -> RestM () -> RestM ()
ifEtagMatches given onTrue onFalse = do
  tag <- eTag
  case tag of
       Nothing -> onFalse
       Just e  -> if eTagMatch e given
                     then onTrue
                     else onFalse
    where eTagMatch :: ETag -> TL.Text -> Bool
          eTagMatch _ "*" = True
          eTagMatch t g   = (any (equalTo t) . map TL.strip . TL.splitOn ",") g
          equalTo (Strong t) g = ("\"" <> t <> "\"") == g
          equalTo (Weak t)   g = ("\"" <> t <> "\"") == g

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
