{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Rest
  (
  -- * REST handler to Scotty
    rest
  -- * Callback result types
  , ProcessingResult(..)
  , Authorized(..) , Moved(..)
  -- * Config
  , RestConfig(..)
  , defaultConfig
  -- * Re-exports
  , StdMethod(..)
  ) where

import           Web.Scotty.Rest.Types

import           Control.Monad             (unless, when, (>=>))
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy      as BS
import           Data.Default.Class        (Default (..), def)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as LE
import           Network.HTTP.Date
import           Network.HTTP.Media        (mapAccept, mapContent, renderHeader)
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

requestMethod :: RestM StdMethod
requestMethod = computeOnce method' $ do
  req <- (RestM . lift) request
  (either (\_ -> stopWith NotImplemented501) return . parseMethod . Wai.requestMethod) req

eTag :: RestM (Maybe ETag)
eTag = computeOnce eTag' (fromConfig generateEtag)

isAvailable :: RestM Bool
isAvailable = computeOnce isAvailable' (fromConfig serviceAvailable)

modificationDate :: RestM (Maybe UTCTime)
modificationDate = computeOnce lastModified' (fromConfig lastModified)

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
     else contentNegotiation

setAllowHeader :: RestM ()
setAllowHeader = do
  allowed <- fromConfig allowedMethods
  setHeader' "allow" . TL.intercalate ", " . map (TL.pack . show) $ allowed

--------------------------------------------------------------------------------
-- OPTIONS
--------------------------------------------------------------------------------

handleOptions :: RestM ()
handleOptions = maybe setAllowHeader runHandler =<< fromConfig optionsHandler

--------------------------------------------------------------------------------
-- Content negotiation
--------------------------------------------------------------------------------

contentNegotiation :: RestM ()
contentNegotiation = do
  config <- retrieve config'
  -- If there is an `Accept` header...
  accept <- return . E.encodeUtf8 . TL.toStrict . fromMaybe "*/*" =<< header' "accept"
  -- ... look at the content types we provide and find and store the best
  -- handler. If we cannot provide that type, stop processing here and return a
  -- NotAcceptable406:
  provided <- contentTypesProvided config
  handler <- maybe (stopWith NotAcceptable406) return (mapAccept provided accept)
  handler' `store` handler

  -- TODO: If there is an `Accept-Language` header, check that we provide that
  -- language. If not → 406.
  -- TODO: If there is an `Accept-Charset` header, check that we provide that
  -- char set. If not → 406.
  -- TODO: Variances

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

--------------------------------------------------------------------------------
-- GET/HEAD
--------------------------------------------------------------------------------

handleGetHeadExisting :: RestM ()
handleGetHeadExisting =
  -- TODO: generate etag
  -- TODO: last modified
  -- TODO: expires
  cached handler' >>= runHandler
  -- TODO: multiple choices

handleGetHeadNonExisting :: RestM () -- FIXME: identical to handleDeleteNonExisting
handleGetHeadNonExisting = handleNonExisting

moved :: RestM ()
moved = do
  fromConfig movedPermanently >>= moved' MovedPermanently301
  fromConfig movedTemporarily >>= moved' MovedTemporarily307
    where moved' _ NotMoved      = return ()
          moved' e (MovedTo url) = setHeader' "location" url >> stopWith e

--------------------------------------------------------------------------------
-- PUT/POST/PATCH
--------------------------------------------------------------------------------

handlePutPostPatchNonExisting :: RestM ()
handlePutPostPatchNonExisting =
  -- TODO: has if-match?
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
pppmethodIsPut = methodIs [PUT]
    (retrieve config' >>= (isConflict >=> (\conflict -> if conflict then stopWith Conflict409 else acceptResource)))
    acceptResource

handlePutPostPatchExisting :: RestM ()
handlePutPostPatchExisting = do
  cond
  method <- requestMethod
  when (method == PUT) $ do
    conflict <- fromConfig isConflict
    when conflict (stopWith Conflict409)

  acceptResource

--------------------------------------------------------------------------------
-- POST/PUT/PATCH, part 2: content-types accepted
--------------------------------------------------------------------------------

acceptResource :: RestM ()
acceptResource = do
  -- Is there a Content-Type header?
  contentTypeHeader <- header' "content-type"
  contentType <- maybe (stopWith UnsupportedMediaType415) (return . E.encodeUtf8 . TL.toStrict) contentTypeHeader

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
  (raw' . LE.encodeUtf8) c
  multiple <- fromConfig multipleChoices
  if multiple
     then status' multipleChoices300
     else status' ok200

--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------

handleDeleteExisting :: RestM ()
handleDeleteExisting = do
  cond
  result <- fromConfig deleteResource
  when (result == NotDeleted) (stopWith (InternalServerError "Could not delete resource"))
  completed <- fromConfig deleteCompleted
  case (result,completed) of
       (Deleted,False)            -> status' accepted202
       (Deleted,True)             -> status' noContent204
       (DeletedWithContent t c,_) -> resourceWithContent t c
       _                          -> stopWith (InternalServerError "“Impossible” state")

handleDeleteNonExisting :: RestM ()
handleDeleteNonExisting = handleNonExisting

--------------------------------------------------------------------------------
-- Conditional requests
--------------------------------------------------------------------------------

cond :: RestM ()
cond = condIfMatch

condIfMatch :: RestM ()
condIfMatch = do
  im <- header' "if-match"
  case im of
       Nothing -> condIfUnmodifiedSince
       Just m  -> eTagMatches m condIfUnmodifiedSince (stopWith PreconditionFailed412)

condIfUnmodifiedSince :: RestM ()
condIfUnmodifiedSince = do
  im <- header' "if-unmodified-since"
  case im of
       Nothing -> condIfNoneMatch
       Just m  -> isModifiedSince m condIfNoneMatch (stopWith PreconditionFailed412)

condIfNoneMatch :: RestM ()
condIfNoneMatch = return () -- TODO

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

handleNonExisting :: RestM ()
handleNonExisting = do
  -- TODO: Has if match? If so: 412

  -- Did this resource exist before?
  existed <- fromConfig previouslyExisted
  unless existed (stopWith NotFound404)

  moved
  stopWith Gone410

setContentTypeHeader :: MediaType -> RestM ()
setContentTypeHeader = setHeader' "content-type" . LE.decodeUtf8 . BS.fromStrict . renderHeader

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

isModifiedSince :: TL.Text -> RestM () -> RestM () -> RestM ()
isModifiedSince given onTrue onFalse = do
  modified <- modificationDate
  case modified of
       Nothing -> onFalse
       Just d  -> if modifiedSince d given
                     then onTrue
                     else onFalse
    where modifiedSince :: UTCTime -> TL.Text -> Bool
          modifiedSince = undefined

handleExcept :: RestException -> ActionT RestException IO ()
handleExcept MovedPermanently301     = status movedPermanently301
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
