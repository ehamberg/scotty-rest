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
  -- * Rest Exceptions
  , RestException(..)
  -- * Re-exports
  , MediaType
  , StdMethod(..)
  , UTCTime
  -- * Utilities
  , toHttpDateHeader
  ) where

import BasePrelude hiding (Handler)

import Web.Scotty.Rest.Types

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
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

type Config m = RestConfig (RestM m)

-- | /rest/ is used where you would use e.g. 'get' in your Scotty app, and
-- will match any method:
--
-- > main = scotty 3000 $ do
-- >   get  "/foo" (text "Hello!")
-- >   rest "/bar" defaultConfig {
-- >       contentTypesProvided = return [("text/html",html "Hello, World!")]
-- >     }
rest :: (MonadIO m) => RoutePattern -> Config m -> ScottyT RestException m ()
rest pattern config = matchAny pattern (restHandlerStart config `rescue` handleExcept)

-- | A 'RestConfig' with default values. To override one or more fields, use
-- record syntax:
--
-- > defaultConfig {
-- >   contentTypesProvided = return [("text/html",html "Hello, World!")]
-- > }
defaultConfig :: (MonadIO m) => Config m
defaultConfig = def

preferred :: (MonadIO m) => Config m -> RestM m (MediaType, RestM m ())
preferred config = do
  -- If there is an `Accept` header -- look at the content types we provide and
  -- find and store the best handler together with the content type.  If we
  -- cannot provide that type, stop processing here and return a
  -- NotAcceptable406:
  accept <- return . convertString . fromMaybe "*/*" =<< header "accept"
  provided <- contentTypesProvided config
  contentType <- maybe (raise NotAcceptable406) return (matchAccept (map fst provided) accept)
  bestHandler <- maybe (raise NotAcceptable406) return (mapAccept provided accept)

  return (contentType, bestHandler)

language :: (MonadIO m) => Config m -> RestM m (Maybe Language)
language config = findPreferred config "accept-language" parse languagesProvided match
  where parse = parseAccept . convertString
        match = flip matches

charset :: (MonadIO m) => Config m -> RestM m (Maybe TL.Text)
charset config = findPreferred config "accept-charset" parse charsetsProvided match
  where parse     = Just
        match a b = TL.toCaseFold a == TL.toCaseFold b

findPreferred :: (MonadIO m) => Config m
              -> TL.Text
              -> (TL.Text -> Maybe a)
              -> (Config m -> RestM m (Maybe [a]))
              -> (a -> a -> Bool)
              -> RestM m (Maybe a)
findPreferred config headerName parse provided match = do
  -- If there is an `Accept-{Charsets,Languages}` header and
  -- `{Charsets,Languages}Provided` is defined, look at what we provide and
  -- find and store the first acceptable one (languages and charsets are in
  -- order of preference). If we cannot provide the requested one, stop
  -- processing here and return a NotAcceptable406.
  headerAndConfig <- runMaybeT $ do
      accept  <- MaybeT (header headerName)
      provide <- MaybeT (provided config)
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
         when (isNothing best) (raise NotAcceptable406)
         return best
  where head' [] = Nothing
        head' (x:_) = Just x

requestMethod :: (MonadIO m) => RestM m StdMethod
requestMethod = do
  req <- request
  case (parseMethod . Wai.requestMethod) req of
       Left  _       -> raise NotImplemented501
       Right method  -> if method `elem` [GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS]
                           then return method
                           else raise NotImplemented501

restHandlerStart :: (MonadIO m) => Config m -> RestM m ()
restHandlerStart config = do
  -- Is our service available?
  available <- serviceAvailable config
  unless available (raise ServiceUnavailable503)

  -- Is the method known?
  method <- requestMethod

  -- TODO: Is the URI too long?

  -- Is the method allowed?
  allowed <- allowedMethods config
  when (method `notElem` allowed) $ do
    setAllowHeader config
    raise MethodNotAllowed405

  -- Is the request malformed?
  isMalformed <- malformedRequest config
  when isMalformed (raise BadRequest400)

  -- Is the client authorized?
  isAuthorized config >>= \case
       Authorized                -> return ()
       (NotAuthorized challenge) -> setHeader "WWW-Authenticate" challenge >> raise Unauthorized401

  -- Is the client forbidden to access this resource?
  isForbidden <- forbidden config
  when isForbidden (raise Forbidden403)

  -- TODO: Are the content headers valid?
  -- TODO: Is the entity length valid?

  if method == OPTIONS
     then handleOptions config
     else contentNegotiationStart config

setAllowHeader :: (MonadIO m) => Config m -> RestM m ()
setAllowHeader config= do
  allowed <- allowedMethods config
  setHeader "allow" . TL.intercalate ", " . map (convertString . show) $ allowed

----------------------------------------------------------------------------------------------------
-- OPTIONS
----------------------------------------------------------------------------------------------------

handleOptions :: (MonadIO m) => Config m -> RestM m ()
handleOptions config = optionsHandler config >>= \case
  Nothing                      -> setAllowHeader config
  (Just (contentType,handler)) -> handler >> setContentTypeHeader contentType

----------------------------------------------------------------------------------------------------
-- Content negotiation
----------------------------------------------------------------------------------------------------

contentNegotiationStart :: (MonadIO m) => Config m -> RestM m ()
contentNegotiationStart = contentNegotiationAccept

contentNegotiationAccept :: (MonadIO m) => Config m -> RestM m ()
contentNegotiationAccept config = do
  accept <- header "accept"
  -- evalute `preferred` to force early 406 (Not acceptable):
  when (isJust accept) (preferred config >> return ())
  contentNegotiationAcceptLanguage config

-- If there is an `Accept-Language` header, check that we provide that
-- language. If not → 406.
contentNegotiationAcceptLanguage :: (MonadIO m) => Config m -> RestM m ()
contentNegotiationAcceptLanguage config = do
  acceptLanguage <- header "accept-language"
  -- evalute `language` to force early 406 (Not acceptable):
  when (isJust acceptLanguage) (language config >> return ())
  contentNegotiationAcceptCharSet config

-- -- If there is an `Accept-Charset` header, check that we provide that
-- -- char set. If not → 406.
contentNegotiationAcceptCharSet :: (MonadIO m) => Config m -> RestM m ()
contentNegotiationAcceptCharSet config = do
  acceptCharset <- header "accept-charset"
  -- evalute `charset` to force early 406 (Not acceptable):
  when (isJust acceptCharset) (charset config >> return ())
  contentNegotiationVariances config

-- If we provide more than one content type, add `Accept` to `Vary` header. If
-- we provide a set of languages and/or charsets, add `Accept-Language` and
-- `Accept-Charset`, respectively, to the `Vary` header too.
contentNegotiationVariances :: (MonadIO m) => Config m -> RestM m ()
contentNegotiationVariances config = do
  ctp <- contentTypesProvided config
  lp  <- languagesProvided config
  cp  <- charsetsProvided config
  varyHeader <- variances config
  let varyHeader'   = if length ctp > 1 then "Accept":varyHeader           else varyHeader
  let varyHeader''  = if isJust lp      then "Accept-Language":varyHeader' else varyHeader'
  let varyHeader''' = if isJust cp      then "Accept-Charset":varyHeader'' else varyHeader''
  setHeader "vary" . TL.intercalate ", " $ varyHeader'''
  checkResourceExists config

checkResourceExists :: (MonadIO m) => Config m -> RestM m ()
checkResourceExists config = do
  method <- requestMethod
  exists <- resourceExists config
  if | method `elem` [GET, HEAD]        -> if exists
                                              then handleGetHeadExisting config
                                              else handleGetHeadNonExisting config
     | method `elem` [PUT, POST, PATCH] -> if exists
                                              then handlePutPostPatchExisting config
                                              else handlePutPostPatchNonExisting config
     | method `elem` [DELETE]           -> if exists
                                              then handleDeleteExisting config
                                              else handleDeleteNonExisting config

----------------------------------------------------------------------------------------------------
-- GET/HEAD
----------------------------------------------------------------------------------------------------

handleGetHeadExisting :: (MonadIO m) => Config m -> RestM m ()
handleGetHeadExisting config = do
  cond config
  addCacheHeaders config
  method <- requestMethod
  (contentType,handler) <- preferred config
  multipleChoices config >>= \case
    MultipleRepresentations t' c' -> writeContent t' c' >> status multipleChoices300
    MultipleWithPreferred t' c' u -> writeContent t' c' >> setHeader "location" u >>  status multipleChoices300
    UniqueRepresentation          -> do when (method == GET) handler
                                        setContentTypeHeader contentType
                                        status ok200

handleGetHeadNonExisting :: (MonadIO m) => Config m -> RestM m ()
handleGetHeadNonExisting = handleNonExisting

checkMoved :: (MonadIO m) => Config m -> RestM m ()
checkMoved config = resourceMoved config >>= \case
  NotMoved               -> return ()
  (MovedPermanently url) -> setHeader "location" url >> raise MovedPermanently301
  (MovedTemporarily url) -> setHeader "location" url >> raise MovedTemporarily307

----------------------------------------------------------------------------------------------------
-- PUT/POST/PATCH
----------------------------------------------------------------------------------------------------

handlePutPostPatchNonExisting :: (MonadIO m) => Config m -> RestM m ()
handlePutPostPatchNonExisting config = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  liftM isJust (header "if-match") >>= (`when` raise PreconditionFailed412)
  ifMethodIs [POST, PATCH]
    (ppppreviouslyExisted config)
    (pppmethodIsPut config)

ppppreviouslyExisted :: (MonadIO m) => Config m -> RestM m ()
ppppreviouslyExisted config = do
  existed <- previouslyExisted config
  if existed
     then pppmovedPermanentlyOrTemporarily config
     else pppmethodIsPost config

pppmovedPermanentlyOrTemporarily :: (MonadIO m) => Config m -> RestM m ()
pppmovedPermanentlyOrTemporarily config = do
  checkMoved config
  ifMethodIs [POST]
    (allowsMissingPost config (acceptResource config) (raise Gone410))
    (pppmethodIsPut config)

pppmethodIsPost :: (MonadIO m) => Config m -> RestM m ()
pppmethodIsPost config =
  ifMethodIs [POST]
    (allowsMissingPost config (pppmethodIsPut config) (raise NotFound404))
    (raise NotFound404)

pppmethodIsPut :: (MonadIO m) => Config m -> RestM m ()
pppmethodIsPut config = do
  method <- requestMethod
  when (method == PUT) $ do
    conflict <- isConflict config
    when conflict (raise Conflict409)

  acceptResource config

handlePutPostPatchExisting :: (MonadIO m) => Config m -> RestM m ()
handlePutPostPatchExisting config = do
  cond config
  pppmethodIsPut config

----------------------------------------------------------------------------------------------------
-- POST/PUT/PATCH, part 2: content-types accepted
----------------------------------------------------------------------------------------------------

acceptResource :: (MonadIO m) => Config m -> RestM m ()
acceptResource config = do
  -- Is there a Content-Type header?
  contentTypeHeader <- header "content-type"
  contentType <- maybe (raise UnsupportedMediaType415) (return . convertString) contentTypeHeader

  -- Do we have a handler for this content type? If so, run it. Alternatively, return 415.
  handlers <- contentTypesAccepted config
  result <- fromMaybe (raise UnsupportedMediaType415) (mapContent handlers contentType)

  exists <- resourceExists config

  case (result, exists) of
       (Failed, _)                        -> status badRequest400
       (Succeeded, True)                  -> status noContent204
       (Succeeded, False)                 -> status created201
       (SucceededWithContent t c, True)   -> resourceWithContent config t c
       (SucceededWithContent t c, False)  -> writeContent t c >> status created201
       (SucceededWithLocation url, True)  -> locationAndResponseCode url noContent204
       (SucceededWithLocation url, False) -> locationAndResponseCode url created201
       (Redirect url, _)                  -> locationAndResponseCode url seeOther303
    where locationAndResponseCode url response = setHeader "location" url >> status response

writeContent :: (MonadIO m) => MediaType -> TL.Text -> RestM m ()
writeContent t c = do
  method <- requestMethod
  setContentTypeHeader t
  when (method /= HEAD) ((raw . convertString) c)

resourceWithContent :: (MonadIO m) => Config m -> MediaType -> TL.Text -> RestM m ()
resourceWithContent config t c = multipleChoices config >>= \case
  UniqueRepresentation          -> setContentTypeHeader t >> (raw . convertString) c >> status ok200
  MultipleRepresentations t' c' -> writeContent t' c' >> status multipleChoices300
  MultipleWithPreferred t' c' u -> writeContent t' c' >> setHeader "location" u >>  status multipleChoices300

----------------------------------------------------------------------------------------------------
-- DELETE
----------------------------------------------------------------------------------------------------

handleDeleteExisting :: (MonadIO m) => Config m -> RestM m ()
handleDeleteExisting config = do
  cond config
  result <- deleteResource config
  case result of
       DeleteEnacted             -> status accepted202
       Deleted                   -> status noContent204
       (DeletedWithResponse t c) -> resourceWithContent config t c
       NotDeleted                -> raise (InternalServerError "Deleting existing resource failed")

handleDeleteNonExisting :: (MonadIO m) => Config m -> RestM m ()
handleDeleteNonExisting = handleNonExisting

----------------------------------------------------------------------------------------------------
-- Conditional requests
----------------------------------------------------------------------------------------------------

cond :: (MonadIO m) => Config m -> RestM m ()
cond = condIfMatch

condIfMatch :: (MonadIO m) => Config m -> RestM m ()
condIfMatch config = header "if-match" >>= \case
  Nothing  -> condIfUnmodifiedSince config
  Just hdr -> ifEtagMatches config hdr
                (condIfUnmodifiedSince config)
                (addEtagHeader config >> raise PreconditionFailed412)

condIfUnmodifiedSince :: (MonadIO m) => Config m -> RestM m ()
condIfUnmodifiedSince config = modifiedSinceHeaderDate config "if-unmodified-since" >>= \case
       Nothing    -> condIfNoneMatch config -- If there are any errors: continue
       Just False -> condIfNoneMatch config
       Just True  -> addLastModifiedHeader config >> raise PreconditionFailed412

condIfNoneMatch :: (MonadIO m) => Config m -> RestM m ()
condIfNoneMatch config = header "if-none-match" >>= \case
  Nothing  -> condIfModifiedSince config
  Just hdr -> ifEtagMatches config hdr
                match
                (condIfModifiedSince config)
    where match = ifMethodIs [GET, HEAD]
                   (notModified config)
                   (addEtagHeader config >> raise PreconditionFailed412)

condIfModifiedSince :: (MonadIO m) => Config m -> RestM m ()
condIfModifiedSince config = modifiedSinceHeaderDate config "if-modified-since" >>= \case
       Nothing    -> return ()
       Just False -> notModified config
       Just True  -> return ()

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

addCacheHeaders :: (MonadIO m) => Config m -> RestM m ()
addCacheHeaders config = do
  addEtagHeader config
  addLastModifiedHeader config
  addExpiresHeader config

notModified :: (MonadIO m) => Config m -> RestM m ()
notModified config = do
  addCacheHeaders config
  raise NotModified304

addEtagHeader :: (MonadIO m) => Config m -> RestM m ()
addEtagHeader config = generateEtag config >>= \case
    Nothing         -> return ()
    Just (Weak t)   -> setHeader "etag" ("W/\"" <> t <> "\"")
    Just (Strong t) -> setHeader "etag" ("\"" <> t <> "\"")

addLastModifiedHeader :: (MonadIO m) => Config m -> RestM m ()
addLastModifiedHeader config = lastModified config >>= \case
    Nothing -> return ()
    Just t  -> setHeader "last-modified" (toHttpDateHeader t)

addExpiresHeader :: (MonadIO m) => Config m -> RestM m ()
addExpiresHeader config = expires config >>= \case
    Nothing  -> return ()
    Just t   -> setHeader "expires" (toHttpDateHeader t)

modifiedSinceHeaderDate :: (MonadIO m) => Config m -> TL.Text -> RestM m (Maybe Bool)
modifiedSinceHeaderDate config hdr = runMaybeT $ do
    modDate    <- MaybeT (lastModified config)
    headerText <- MaybeT (header hdr)
    headerDate <- MaybeT (return (parseHeaderDate headerText))
    return (modDate > headerDate)

handleNonExisting :: (MonadIO m) => Config m -> RestM m ()
handleNonExisting config = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  liftM isJust (header "if-match") >>= (`when` raise PreconditionFailed412)

  -- Did this resource exist before?
  existed <- previouslyExisted config
  unless existed (raise NotFound404)

  checkMoved config
  raise Gone410

setContentTypeHeader :: (MonadIO m) => MediaType -> RestM m ()
setContentTypeHeader = setHeader "content-type" . convertString . renderHeader

ifMethodIs :: (MonadIO m) => [StdMethod] -> RestM m () -> RestM m () -> RestM m ()
ifMethodIs ms onTrue onFalse = do
  method <- requestMethod
  if method `elem` ms
     then onTrue
     else onFalse

allowsMissingPost :: (MonadIO m) => Config m -> RestM m () -> RestM m () -> RestM m ()
allowsMissingPost config onTrue onFalse = do
  allowed <- allowMissingPost config
  if allowed
     then onTrue
     else onFalse

ifEtagMatches :: (MonadIO m) => Config m -> TL.Text -> RestM m () -> RestM m () -> RestM m ()
ifEtagMatches config given onTrue onFalse = do
  tag <- generateEtag config
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

-- | Formats a 'UTCTime' as a HTTP date, e.g. /Sun, 06 Nov 1994 08:49:37 GMT/.
toHttpDateHeader :: UTCTime -> TL.Text
toHttpDateHeader = convertString . formatHTTPDate . epochTimeToHTTPDate . convert

handleExcept :: (Monad m) => RestException -> RestM m ()
handleExcept MovedPermanently301     = status movedPermanently301
handleExcept NotModified304          = status notModified304
handleExcept MovedTemporarily307     = status temporaryRedirect307
handleExcept BadRequest400           = status badRequest400
handleExcept Unauthorized401         = status unauthorized401
handleExcept Forbidden403            = status forbidden403
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
