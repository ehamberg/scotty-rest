{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}

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

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Web.Scotty.Trans
import Network.HTTP.Date
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types (parseMethod)
import Network.HTTP.Types.Status
import Network.HTTP.Media (MediaType, mapAccept, mapContent, renderHeader)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import Network.Wai (Request, requestMethod)
import qualified Data.ByteString.Lazy as BS
import Data.String (fromString)
import Data.Default.Class (Default(..), def)
import Control.Applicative
import Control.Monad.State (evalStateT)
import Control.Monad.Reader
import Lens.Family2
import Lens.Family2.TH (makeLensesBy)
import Lens.Family2.State

newtype RestM a = RestM
  { runRestM :: ReaderT RestConfig (StateT RequestState (ActionT RestException IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState RequestState, MonadReader RestConfig)

type Handler = ActionT RestException IO

type Url = TL.Text
type Challenge = TL.Text

data Moved = NotMoved | MovedTo Url
data ETag = Strong TL.Text
          | Weak TL.Text
data ProcessingResult = Succeeded
                      | SucceededWithContent MediaType TL.Text
                      | SucceededSeeOther Url
                      | SucceededWithLocation Url
                      | Failed
data DeleteResult = NotDeleted
                  | Deleted
                  | DeletedWithContent MediaType TL.Text
                  deriving Eq
data Authorized = Authorized | NotAuthorized Challenge

instance Default RequestState where
  def = RequestState def def def def def

data RequestState = RequestState
  { _method      :: (Maybe StdMethod)       -- Method
  , _handler     :: (Maybe (Handler ()))    -- Handler found
  , _newResource :: (Maybe Bool)            -- New resource?
  , _eTag        :: (Maybe (Maybe ETag))    -- ETag, if computed
  , _lastModified :: (Maybe (Maybe UTCTime)) -- Last modified, if computed
  }

data RestConfig = RestConfig
  { allowedMethods       :: RestM [StdMethod]
  , resourceExists       :: RestM Bool
  , previouslyExisted    :: RestM Bool
  , isConflict           :: RestM Bool
  , contentTypesAccepted :: RestM [(MediaType, Handler ProcessingResult)]
  , contentTypesProvided :: RestM [(MediaType, Handler ())]
  , deleteResource       :: RestM DeleteResult
  , deleteCompleted      :: RestM Bool
  , optionsHandler       :: RestM (Maybe (Handler ()))
  , charSetsProvided     :: RestM (Maybe [TL.Text])
  , generateEtag         :: RestM (Maybe ETag)
  , lastModified         :: RestM (Maybe UTCTime)
  , isAuthorized         :: RestM Authorized
  , serviceAvailable     :: RestM Bool
  , allowMissingPost     :: RestM Bool
  , multipleChoices      :: RestM Bool
  , movedPermanently     :: RestM Moved
  , movedTemporarily     :: RestM Moved
  }

instance Default RestConfig where
 def = RestConfig { allowedMethods       = return [GET, HEAD, OPTIONS]
                  , resourceExists       = return True
                  , previouslyExisted    = return False
                  , isConflict           = return False
                  , contentTypesAccepted = return []
                  , contentTypesProvided = return []
                  , deleteResource       = return NotDeleted
                  , deleteCompleted      = return True
                  , optionsHandler       = return Nothing
                  , charSetsProvided     = return Nothing
                  , generateEtag         = return Nothing
                  , lastModified         = return Nothing
                  , isAuthorized         = return Authorized
                  , serviceAvailable     = return True
                  , allowMissingPost     = return True
                  , multipleChoices      = return False
                  , movedPermanently     = return NotMoved
                  , movedTemporarily     = return NotMoved
                  }

data RestException = MovedPermanently301
                   | MovedTemporarily307
                   | BadRequest400
                   | Unauthorized401
                   | NotFound404
                   | NotAcceptable406
                   | Conflict409
                   | Gone410
                   | PreconditionFailed412
                   | UnsupportedMediaType415
                   | NotImplemented501
                   | ServiceUnavailable503
                   | MethodNotAllowed405
                   | InternalServerError TL.Text
                   deriving (Show, Eq)

instance ScottyError RestException where
  stringError = InternalServerError . TL.pack
  showError = fromString . show

$(makeLensesBy (\n -> Just ((tail n) ++ "'")) ''RequestState)

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
  let run = evalStateT (runReaderT (runRestM restHandlerStart) config) def
  run `rescue` handleExcept

stopWith :: RestException -> RestM a
stopWith = RestM . lift . lift . raise

runHandler :: Handler a -> RestM a
runHandler = RestM . lift .lift

setHeader' :: TL.Text -> TL.Text -> RestM ()
setHeader' h v = RestM . lift . lift $ setHeader h v

request' :: RestM Request
request' = RestM . lift . lift $ request

header' :: TL.Text -> RestM (Maybe TL.Text)
header' = RestM . lift . lift . header

status' :: Status -> RestM ()
status' = RestM . lift . lift . status

raw' :: BS.ByteString -> RestM ()
raw' = RestM . lift . lift .raw

restHandlerStart :: RestM ()
restHandlerStart = do
  config <- ask
  -- Is our service available?
  available <- serviceAvailable config
  unless available (stopWith ServiceUnavailable503)

  -- Is the method known?
  method <- either (\_ -> stopWith NotImplemented501) return . parseMethod . requestMethod =<< request'
  method' .= Just method

  -- TODO: Is the URI too long?

  -- Is the method allowed?
  allowed <- allowedMethods config
  when (method `notElem` allowed) $ do
    setAllowHeader
    stopWith MethodNotAllowed405

  -- TODO: Is the request malformed?

  -- Is the client authorized?
  isAuthorized config >>= \case
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
  config <- ask
  setHeader' "allow" . TL.intercalate ", " . map (TL.pack . show) =<< allowedMethods config

--------------------------------------------------------------------------------
-- OPTIONS
--------------------------------------------------------------------------------

handleOptions :: RestM ()
handleOptions = do
  config <- ask
  maybe setAllowHeader runHandler =<< optionsHandler config

--------------------------------------------------------------------------------
-- Content negotiation
--------------------------------------------------------------------------------

contentNegotiation :: RestM ()
contentNegotiation = do
  config <- ask
  -- If there is an `Accept` header...
  accept <- return . E.encodeUtf8 . TL.toStrict . fromMaybe "*/*" =<< header' "accept"
  -- ... look at the content types we provide and find and store the best
  -- handler. If we cannot provide that type, stop processing here and return a
  -- NotAcceptable406:
  provided <- contentTypesProvided config
  handler <- maybe (stopWith NotAcceptable406) return (mapAccept provided accept)
  handler' .= Just handler

  -- TODO: If there is an `Accept-Language` header, check that we provide that
  -- language. If not → 406.
  -- TODO: If there is an `Accept-Charset` header, check that we provide that
  -- char set. If not → 406.
  -- TODO: Variances

  checkResourceExists

checkResourceExists :: RestM ()
checkResourceExists = do
  config <- ask
  method <- fromCache method'
  exists <- resourceExists config
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
  fromCache handler' >>= runHandler
  -- TODO: multiple choices

handleGetHeadNonExisting :: RestM ()
handleGetHeadNonExisting = do
  config <- ask
  -- TODO: Has if match? If so: 412

  -- Did this resource exist before?
  existed <- previouslyExisted config
  unless existed (stopWith NotFound404)

  moved
  stopWith Gone410

moved :: RestM ()
moved = do
  config <- ask
  movedPermanently config >>= moved' MovedPermanently301
  movedTemporarily config >>= moved' MovedTemporarily307
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
  config <- ask
  existed <- previouslyExisted config
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
    (ask >>= (isConflict >=> (\conflict -> if conflict then stopWith Conflict409 else acceptResource)))
    acceptResource

handlePutPostPatchExisting :: RestM ()
handlePutPostPatchExisting = do
  config <- ask
  cond
  method <- fromCache method'
  when (method == PUT) $ do
    conflict <- isConflict config
    when conflict (stopWith Conflict409)

  acceptResource

--------------------------------------------------------------------------------
-- POST/PUT/PATCH, part 2: content-types accepted
--------------------------------------------------------------------------------

acceptResource :: RestM ()
acceptResource = do
  config <- ask
  -- Is there a Content-Type header?
  contentTypeHeader <- header' "content-type"
  contentType <- maybe (stopWith UnsupportedMediaType415) (return . E.encodeUtf8 . TL.toStrict) contentTypeHeader

  -- Do we have a handler for this content type? If so, run it. Alternatively, return 415.
  handlers <- contentTypesAccepted config
  result <- maybe (stopWith UnsupportedMediaType415) runHandler (mapContent handlers contentType)

  case result of
       Failed                    -> status' badRequest400
       Succeeded                 -> status' noContent204
       SucceededSeeOther url     -> resourceSeeOther url
       SucceededWithLocation url -> resourceWithLocation url
       SucceededWithContent t c  -> resourceWithContent t c

resourceSeeOther :: Url -> RestM ()
resourceSeeOther url = do
  newResource <- fromCache newResource'
  if newResource
     then status' created201
     else setHeader' "location" url >> status' seeOther303

resourceWithLocation :: Url -> RestM ()
resourceWithLocation url = do
  newResource <- fromCache newResource'
  if newResource
     then do setHeader' "location" url
             status' created201
     else status' noContent204

-- C
resourceWithContent :: MediaType -> TL.Text -> RestM ()
resourceWithContent t c = do
  config <- ask
  setContentTypeHeader t
  (raw' . LE.encodeUtf8) c
  multiple <- multipleChoices config
  if multiple
     then status' multipleChoices300
     else status' ok200

--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------

handleDeleteExisting :: RestM ()
handleDeleteExisting = do
  config <- ask
  cond
  result <- deleteResource config
  when (result == NotDeleted) (stopWith (InternalServerError "Could not delete resource"))
  completed <- deleteCompleted config
  case (result,completed) of
       (Deleted,False)            -> status' accepted202
       (Deleted,True)             -> status' noContent204
       (DeletedWithContent t c,_) -> resourceWithContent t c
       _                          -> stopWith (InternalServerError "“Impossible” state")

handleDeleteNonExisting :: RestM ()
handleDeleteNonExisting = do
  config <- ask
  -- TODO: has if-match?
  existed <- previouslyExisted config
  unless existed (stopWith NotFound404)
  moved
  stopWith Gone410

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

setContentTypeHeader :: MediaType -> RestM ()
setContentTypeHeader = setHeader' "content-type" . LE.decodeUtf8 . BS.fromStrict . renderHeader

methodIs :: [StdMethod] -> RestM () -> RestM () -> RestM ()
methodIs ms onTrue onFalse = do
  m <- fromCache method'
  if m `elem` ms
     then onTrue
     else onFalse

allowsMissingPost :: RestM () -> RestM () -> RestM ()
allowsMissingPost onTrue onFalse = do
  config <- ask
  allowed <- allowMissingPost config
  if allowed
     then newResource' .= Just True >> onTrue
     else onFalse

eTagMatches :: TL.Text -> RestM () -> RestM () -> RestM ()
eTagMatches given onTrue onFalse = do
  eTag <- use eTag' >>= \case Just e  -> return e
                              Nothing -> do config <- ask
                                            tag <- generateEtag config
                                            eTag' .= Just tag
                                            return tag
  case eTag of
       Nothing -> onFalse
       Just e  -> if eTagMatch e given
                     then onTrue
                     else onFalse
    where eTagMatch :: ETag -> TL.Text -> Bool
          eTagMatch = undefined

isModifiedSince :: TL.Text -> RestM () -> RestM () -> RestM ()
isModifiedSince given onTrue onFalse = do
  modified <- use lastModified' >>= \case Just e  -> return e
                                          Nothing -> do config <- ask
                                                        date <- lastModified config
                                                        lastModified' .= Just date
                                                        return date
  case modified of
       Nothing -> onFalse
       Just d  -> if modifiedSince d given
                     then onTrue
                     else onFalse
    where modifiedSince :: UTCTime -> TL.Text -> Bool
          modifiedSince = undefined

fromCache :: Lens' RequestState (Maybe a) -> RestM a
fromCache lens = use lens >>= \case
  Just v  -> return v
  Nothing -> stopWith (InternalServerError "Cached state variable missing")

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
