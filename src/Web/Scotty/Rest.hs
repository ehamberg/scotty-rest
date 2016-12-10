{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Rest
  (
  -- * REST monad transformer
    RestT
  -- * REST handler to Scotty
  , rest
  -- * Callback result types
  , Authorized(..)
  , DeleteResult(..)
  , ETag(..)
  , Moved(..)
  , ProcessingResult(..)
  , Representation(..)
  -- * Config
  , EndpointConfig(..)
  , defaultConfig
  -- * Rest Exceptions
  , RestException(..)
  -- * Re-exports
  , MediaType
  , StdMethod(..)
  , UTCTime
  -- * Utilities
  , toHttpDateHeader
  , requestMethod
  ) where

import BasePrelude hiding (Handler)

import Web.Scotty.Rest.Types

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Convertible          (convert)
import           Data.String.Conversions   (cs)
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

type Config m = EndpointConfig (RestT m)

-- | /rest/ is used where you would use e.g. 'get' in your Scotty app, and
-- will match any method:
--
-- > main = scotty 3000 $ do
-- >   get  "/foo" (text "Hello!")
-- >   rest "/bar" defaultConfig {
-- >       contentTypesProvided = return [("text/html", html "Hello, World!")]
-- >     }
rest :: (MonadIO m) => RoutePattern -> Config m -> ScottyT RestException m ()
rest pattern config = matchAny pattern (restHandlerStart config `rescue` handleExcept)

-- | A 'RestConfig' with default values. To override one or more fields, use
-- record syntax:
--
-- > defaultConfig {
-- >   contentTypesProvided = return [("text/html", html "Hello, World!")]
-- > }
defaultConfig :: (Monad m) => Config m
defaultConfig = EndpointConfig {
                    allowedMethods       = return [GET, HEAD, OPTIONS]
                  , resourceExists       = return True
                  , previouslyExisted    = return False
                  , isConflict           = return False
                  , contentTypesAccepted = return []
                  , contentTypesProvided = return []
                  , languagesProvided    = return Nothing
                  , charsetsProvided     = return Nothing
                  , deleteResource       = return NotDeleted
                  , optionsHandler       = return Nothing
                  , generateEtag         = return Nothing
                  , expires              = return Nothing
                  , lastModified         = return Nothing
                  , malformedRequest     = return False
                  , isAuthorized         = return Authorized
                  , forbidden            = return False
                  , serviceAvailable     = return True
                  , allowMissingPost     = return True
                  , multipleChoices      = return UniqueRepresentation
                  , resourceMoved        = return NotMoved
                  , variances            = return []
                  }


preferred :: (Monad m) => Config m -> RestT m (MediaType, RestT m ())
preferred config = do
  -- If there is an `Accept` header – look at the content types we provide and
  -- find and store the best handler together with the content type. If we
  -- cannot provide that type, stop processing here and return a
  -- NotAcceptable406:
  accept <- (cs . fromMaybe "*/*") <$> header "accept"
  provided <- contentTypesProvided config
  contentType <- maybe (raise NotAcceptable406) return (matchAccept (map fst provided) accept)
  bestHandler <- maybe (raise NotAcceptable406) return (mapAccept provided accept)

  return (contentType, bestHandler)

language :: (Monad m) => Config m -> RestT m (Maybe Language)
language config = findPreferred config "accept-language" parse languagesProvided match
  where parse = parseAccept . cs
        match = flip matches

charset :: (Monad m) => Config m -> RestT m (Maybe TL.Text)
charset config = findPreferred config "accept-charset" parse charsetsProvided match
  where parse     = Just
        match a b = TL.toCaseFold a == TL.toCaseFold b

findPreferred :: (Monad m) => Config m
              -> TL.Text
              -> (TL.Text -> Maybe a)
              -> (Config m -> RestT m (Maybe [a]))
              -> (a -> a -> Bool)
              -> RestT m (Maybe a)
findPreferred config headerName parse provided match = do
  -- If there is an `Accept-{Charsets,Languages}` header and
  -- `{Charsets,Languages}Provided` is defined, look at what we provide and
  -- find and store the first acceptable one (languages and charsets are in
  -- order of preference). If we cannot provide the requested one, stop
  -- processing here and return a NotAcceptable406.
  headerAndConfig <- runMaybeT $ do
      accept  <- MaybeT (header headerName)
      provide <- MaybeT (provided config)
      return (accept, provide)
  case headerAndConfig of
       Nothing    -> return Nothing
       Just (a, p) -> do
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

checkRequestMethod :: (Monad m) => RestT m ()
checkRequestMethod = do
  method <- requestMethod
  unless (method `elem` [GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS]) (raise NotImplemented501)

restHandlerStart :: (Monad m) => Config m -> RestT m ()
restHandlerStart config = do
  -- Is our service available?
  available <- serviceAvailable config
  unless available (raise ServiceUnavailable503)

  -- Is the method known?
  checkRequestMethod

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
       (NotAuthorized challenge) -> do setHeader "WWW-Authenticate" challenge
                                       raise Unauthorized401

  -- Is the client forbidden to access this resource?
  isForbidden <- forbidden config
  when isForbidden (raise Forbidden403)

  -- TODO: Are the content headers valid?
  -- TODO: Is the entity length valid?

  if method == OPTIONS
     then handleOptions config
     else contentNegotiationStart config

setAllowHeader :: (Monad m) => Config m -> RestT m ()
setAllowHeader config= do
  allowed <- allowedMethods config
  setHeader "Allow" . TL.intercalate ", " . map (cs . show) $ allowed

----------------------------------------------------------------------------------------------------
-- OPTIONS
----------------------------------------------------------------------------------------------------

handleOptions :: (Monad m) => Config m -> RestT m ()
handleOptions config = optionsHandler config >>= \case
  Nothing                       -> setAllowHeader config
  (Just (contentType, handler)) -> handler >> setContentTypeHeader contentType

----------------------------------------------------------------------------------------------------
-- Content negotiation
----------------------------------------------------------------------------------------------------

contentNegotiationStart :: (Monad m) => Config m -> RestT m ()
contentNegotiationStart = contentNegotiationAccept

contentNegotiationAccept :: (Monad m) => Config m -> RestT m ()
contentNegotiationAccept config = do
  accept <- header "accept"
  -- evalute `preferred` to force early 406 (Not acceptable):
  when (isJust accept) $ void (preferred config)
  contentNegotiationAcceptLanguage config

-- If there is an `Accept-Language` header, check that we provide that
-- language. If not → 406.
contentNegotiationAcceptLanguage :: (Monad m) => Config m -> RestT m ()
contentNegotiationAcceptLanguage config = do
  acceptLanguage <- header "accept-language"
  -- evalute `language` to force early 406 (Not acceptable):
  when (isJust acceptLanguage) $ void (language config)
  contentNegotiationAcceptCharSet config

-- -- If there is an `Accept-Charset` header, check that we provide that
-- -- char set. If not → 406.
contentNegotiationAcceptCharSet :: (Monad m) => Config m -> RestT m ()
contentNegotiationAcceptCharSet config = do
  acceptCharset <- header "accept-charset"
  -- evalute `charset` to force early 406 (Not acceptable):
  when (isJust acceptCharset) $ void (charset config)
  contentNegotiationVariances config

-- If we provide more than one content type, add `Accept` to `Vary` header. If
-- we provide a set of languages and/or charsets, add `Accept-Language` and
-- `Accept-Charset`, respectively, to the `Vary` header too.
contentNegotiationVariances :: (Monad m) => Config m -> RestT m ()
contentNegotiationVariances config = do
  ctp <- contentTypesProvided config
  lp  <- languagesProvided config
  cp  <- charsetsProvided config
  varyHeader <- variances config
  let varyHeader'   = if length ctp > 1 then "Accept":varyHeader           else varyHeader
  let varyHeader''  = if isJust lp      then "Accept-Language":varyHeader' else varyHeader'
  let varyHeader''' = if isJust cp      then "Accept-Charset":varyHeader'' else varyHeader''
  unless (null varyHeader''') $ setHeader "Vary" . TL.intercalate ", " $ varyHeader'''
  checkResourceExists config

checkResourceExists :: (Monad m) => Config m -> RestT m ()
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

handleGetHeadExisting :: (Monad m) => Config m -> RestT m ()
handleGetHeadExisting config = do
  cond config
  addCacheHeaders config
  method <- requestMethod
  (contentType, handler) <- preferred config
  multipleChoices config >>= \case
    MultipleRepresentations t' c' -> do writeContent t' c'
                                        status multipleChoices300
    MultipleWithPreferred t' c' u -> do writeContent t' c'
                                        setHeader "Location" u
                                        status multipleChoices300
    UniqueRepresentation          -> do when (method == GET) handler
                                        setContentTypeHeader contentType
                                        status ok200

handleGetHeadNonExisting :: (Monad m) => Config m -> RestT m ()
handleGetHeadNonExisting = handleNonExisting

checkMoved :: (Monad m) => Config m -> RestT m ()
checkMoved config = resourceMoved config >>= \case
  NotMoved               -> return ()
  (MovedPermanently url) -> setHeader "Location" url >> raise MovedPermanently301
  (MovedTemporarily url) -> setHeader "Location" url >> raise MovedTemporarily307

----------------------------------------------------------------------------------------------------
-- PUT/POST/PATCH
----------------------------------------------------------------------------------------------------

handlePutPostPatchNonExisting :: (Monad m) => Config m -> RestT m ()
handlePutPostPatchNonExisting config = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  hasIfMatchHeader <- isJust <$> header "if-match"
  when hasIfMatchHeader (raise PreconditionFailed412)
  ifMethodIn [POST, PATCH]
    (ppppreviouslyExisted config)
    (pppmethodIsPut config)

ppppreviouslyExisted :: (Monad m) => Config m -> RestT m ()
ppppreviouslyExisted config = do
  existed <- previouslyExisted config
  if existed
     then pppmovedPermanentlyOrTemporarily config
     else pppmethodIsPost config

pppmovedPermanentlyOrTemporarily :: (Monad m) => Config m -> RestT m ()
pppmovedPermanentlyOrTemporarily config = do
  checkMoved config
  ifMethodIn [POST]
    (allowsMissingPost config (acceptResource config) (raise Gone410))
    (pppmethodIsPut config)

pppmethodIsPost :: (Monad m) => Config m -> RestT m ()
pppmethodIsPost config =
  ifMethodIn [POST]
    (allowsMissingPost config (pppmethodIsPut config) (raise NotFound404))
    (raise NotFound404)

pppmethodIsPut :: (Monad m) => Config m -> RestT m ()
pppmethodIsPut config = do
  method <- requestMethod
  when (method == PUT || method == PATCH) $ do
    conflict <- isConflict config
    when conflict (raise Conflict409)

  acceptResource config

handlePutPostPatchExisting :: (Monad m) => Config m -> RestT m ()
handlePutPostPatchExisting config = do
  cond config
  pppmethodIsPut config

----------------------------------------------------------------------------------------------------
-- POST/PUT/PATCH, part 2: content-types accepted
----------------------------------------------------------------------------------------------------

acceptResource :: (Monad m) => Config m -> RestT m ()
acceptResource config = do
  -- Is there a Content-Type header?
  contentTypeHeader <- header "Content-Type"
  contentType <- maybe (raise UnsupportedMediaType415) (return . cs) contentTypeHeader

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
    where locationAndResponseCode url response = setHeader "Location" url >> status response

writeContent :: (Monad m) => MediaType -> TL.Text -> RestT m ()
writeContent t c = do
  method <- requestMethod
  setContentTypeHeader t
  when (method /= HEAD) ((raw . cs) c)

resourceWithContent :: (Monad m) => Config m -> MediaType -> TL.Text -> RestT m ()
resourceWithContent config t c = multipleChoices config >>= \case
  UniqueRepresentation          -> do setContentTypeHeader t
                                      (raw . cs) c
                                      status ok200
  MultipleRepresentations t' c' -> do writeContent t' c'
                                      status multipleChoices300
  MultipleWithPreferred t' c' u -> do writeContent t' c'
                                      setHeader "Location" u
                                      status multipleChoices300

----------------------------------------------------------------------------------------------------
-- DELETE
----------------------------------------------------------------------------------------------------

handleDeleteExisting :: (Monad m) => Config m -> RestT m ()
handleDeleteExisting config = do
  cond config
  result <- deleteResource config
  case result of
       DeleteEnacted             -> status accepted202
       Deleted                   -> status noContent204
       (DeletedWithResponse t c) -> resourceWithContent config t c
       NotDeleted                -> raise (InternalServerError "Deleting existing resource failed")

handleDeleteNonExisting :: (Monad m) => Config m -> RestT m ()
handleDeleteNonExisting = handleNonExisting

----------------------------------------------------------------------------------------------------
-- Conditional requests
----------------------------------------------------------------------------------------------------

cond :: (Monad m) => Config m -> RestT m ()
cond = condIfMatch

condIfMatch :: (Monad m) => Config m -> RestT m ()
condIfMatch config = header "if-match" >>= \case
  Nothing  -> condIfUnmodifiedSince config
  Just hdr -> ifEtagMatches config hdr
                (condIfUnmodifiedSince config)
                (addEtagHeader config >> raise PreconditionFailed412)

condIfUnmodifiedSince :: (Monad m) => Config m -> RestT m ()
condIfUnmodifiedSince config = modifiedSinceHeaderDate config "if-unmodified-since" >>= \case
       Nothing    -> condIfNoneMatch config -- If there are any errors: continue
       Just False -> condIfNoneMatch config
       Just True  -> addLastModifiedHeader config >> raise PreconditionFailed412

condIfNoneMatch :: (Monad m) => Config m -> RestT m ()
condIfNoneMatch config = header "if-none-match" >>= \case
  Nothing  -> condIfModifiedSince config
  Just hdr -> ifEtagMatches config hdr
                match
                (condIfModifiedSince config)
    where match = ifMethodIn [GET, HEAD]
                   (notModified config)
                   (addEtagHeader config >> raise PreconditionFailed412)

condIfModifiedSince :: (Monad m) => Config m -> RestT m ()
condIfModifiedSince config = modifiedSinceHeaderDate config "if-modified-since" >>= \case
       Nothing    -> return ()
       Just False -> notModified config
       Just True  -> return ()

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

addCacheHeaders :: (Monad m) => Config m -> RestT m ()
addCacheHeaders config = do
  addEtagHeader config
  addLastModifiedHeader config
  addExpiresHeader config

notModified :: (Monad m) => Config m -> RestT m ()
notModified config = do
  addCacheHeaders config
  raise NotModified304

addEtagHeader :: (Monad m) => Config m -> RestT m ()
addEtagHeader config = generateEtag config >>= \case
    Nothing         -> return ()
    Just (Weak t)   -> setHeader "Etag" ("W/\"" <> t <> "\"")
    Just (Strong t) -> setHeader "Etag" ("\"" <> t <> "\"")

addLastModifiedHeader :: (Monad m) => Config m -> RestT m ()
addLastModifiedHeader config = lastModified config >>= \case
    Nothing -> return ()
    Just t  -> setHeader "Last-Modified" (toHttpDateHeader t)

addExpiresHeader :: (Monad m) => Config m -> RestT m ()
addExpiresHeader config = expires config >>= \case
    Nothing  -> return ()
    Just t   -> setHeader "Expires" (toHttpDateHeader t)

modifiedSinceHeaderDate :: (Monad m) => Config m -> TL.Text -> RestT m (Maybe Bool)
modifiedSinceHeaderDate config hdr = runMaybeT $ do
    modDate    <- MaybeT (lastModified config)
    headerText <- MaybeT (header hdr)
    headerDate <- MaybeT (return (parseHeaderDate headerText))
    return (modDate > headerDate)

handleNonExisting :: (Monad m) => Config m -> RestT m ()
handleNonExisting config = do
  -- If there is an if-match header, the precondition failed since the resource doesn't exist
  hasIfMatchHeader <- isJust <$> header "if-match"
  when hasIfMatchHeader (raise PreconditionFailed412)

  -- Did this resource exist before?
  existed <- previouslyExisted config
  unless existed (raise NotFound404)

  checkMoved config
  raise Gone410

setContentTypeHeader :: (Monad m) => MediaType -> RestT m ()
setContentTypeHeader = setHeader "Content-Type" . cs . renderHeader

ifMethodIn :: (Monad m) => [StdMethod] -> RestT m () -> RestT m () -> RestT m ()
ifMethodIn ms onTrue onFalse = do
  method <- requestMethod
  if method `elem` ms
     then onTrue
     else onFalse

allowsMissingPost :: (Monad m) => Config m -> RestT m () -> RestT m () -> RestT m ()
allowsMissingPost config onTrue onFalse = do
  allowed <- allowMissingPost config
  if allowed
     then onTrue
     else onFalse

ifEtagMatches :: (Monad m) => Config m -> TL.Text -> RestT m () -> RestT m () -> RestT m ()
ifEtagMatches _      "*"   onTrue _       = onTrue
ifEtagMatches config given onTrue onFalse = do
  tag <- generateEtag config
  case tag of
       Nothing -> onFalse
       Just e  -> if eTagMatch e given
                     then onTrue
                     else onFalse
    where eTagMatch :: ETag -> TL.Text -> Bool
          eTagMatch t g   = (any (equalTo t) . map TL.strip . TL.splitOn ",") g
          equalTo (Strong t) g = ("\"" <> t <> "\"") == g
          equalTo (Weak t)   g = ("\"" <> t <> "\"") == g

parseHeaderDate :: TL.Text -> Maybe UTCTime
parseHeaderDate hdr = do
  headerDate <- (parseHTTPDate . cs) hdr
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
toHttpDateHeader = cs . formatHTTPDate . epochTimeToHTTPDate . convert

-- | Returns the method used for the current request, e.g. /POST/.
requestMethod :: (Monad m) => RestT m StdMethod
requestMethod = do
  req <- request
  case (parseMethod . Wai.requestMethod) req of
    Right method -> return method
    Left method  -> raise (InternalServerError ("Parsing method " <> cs method <> " failed"))


handleExcept :: (Monad m) => RestException -> RestT m ()
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
