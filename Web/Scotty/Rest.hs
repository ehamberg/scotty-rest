{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
{-# Language MultiWayIf #-}

module Web.Scotty.Rest
( RestConfig(..)
, RestException(..)
, ProcessingResult(..)
, Authorized(..)
, Moved(..)
, defaultConfig
, rest
, StdMethod(..)
) where

import Data.Maybe (fromMaybe)
import Web.Scotty.Trans hiding (get)
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
import Control.Monad.State
import Control.Monad.Reader

newtype RestM a = RestM
  { runRestM :: ReaderT RestConfig (StateT RequestState (ActionT RestException IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState RequestState, MonadReader RestConfig)

type Handler = ActionT RestException IO

type Url = TL.Text
type Challenge = TL.Text

data Moved = NotMoved | MovedTo Url
data ProcessingResult = Succeeded
                      | SucceededWithContent MediaType TL.Text
                      | SucceededWithUrl Url
                      | Failed
data Authorized = Authorized | NotAuthorized Challenge

data RequestState = RequestState
  { _method :: Maybe StdMethod
  , _handler :: Maybe (Handler ())
  }



instance Default RequestState where
  def = RequestState Nothing Nothing

data RestConfig = RestConfig
  { allowedMethods       :: RestM [StdMethod]
  , resourceExists       :: RestM Bool
  , previouslyExisted    :: RestM Bool
  , isConflict           :: RestM Bool
  , contentTypesAccepted :: RestM [(MediaType, Handler ProcessingResult)]
  , contentTypesProvided :: RestM [(MediaType, Handler ())]
  , optionsHandler       :: RestM (Maybe (Handler ()))
  , charSetsProvided     :: RestM (Maybe [TL.Text])
  , isAuthorized         :: RestM Authorized
  , serviceAvailable     :: RestM Bool
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
                  , optionsHandler       = return Nothing
                  , charSetsProvided     = return Nothing
                  , isAuthorized         = return Authorized
                  , serviceAvailable     = return True
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
                   | UnsupportedMediaType415
                   | NotImplemented501
                   | ServiceUnavailable503
                   | MethodNotAllowed405
                   | InternalServerError TL.Text
                   deriving (Show, Eq)

instance ScottyError RestException where
  stringError = InternalServerError . TL.pack
  showError = fromString . show

defaultConfig :: RestConfig
defaultConfig = def

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

  ---- Is the method known?
  method <- either (\_ -> stopWith NotImplemented501) return . parseMethod . requestMethod =<< request'
  modify (\s -> s{_method = Just method})

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

handleOptions :: RestM ()
handleOptions = do
  config <- ask
  maybe setAllowHeader runHandler =<< optionsHandler config

contentNegotiation :: RestM ()
contentNegotiation = do
  config <- ask
  -- If there is an `Accept` header, stop processing here and return a
  -- NotAcceptable406 exception if we cannot provide that type:
  accept <- return . E.encodeUtf8 . TL.toStrict . fromMaybe "*/*" =<< header' "accept"
  provided <- contentTypesProvided config
  handler <- maybe (stopWith NotAcceptable406) return (mapAccept provided accept)
  modify (\s -> s{_handler = Just handler})

  -- TODO: If there is an `Accept-Language` header, check that we provide that
  -- language. If not → 406.
  -- TODO: If there is an `Accept-Charset` header, check that we provide that
  -- char set. If not → 406.
  -- TODO: Variances

  checkResourceExists

checkResourceExists :: RestM ()
checkResourceExists = do
  config <- ask
  method <- getCached _method
  exists <- resourceExists config
  if | method `elem` [GET, HEAD]        -> if exists
                                              then handleGetHeadExisting
                                              else handleGetHeadNonExisting
     | method `elem` [PUT, POST, PATCH] -> if exists
                                              then handlePutPostPatchExisting
                                              else handlePutPostPatchNonExisting

handleGetHeadExisting :: RestM ()
handleGetHeadExisting = do
  -- TODO: generate etag
  -- TODO: last modified
  -- TODO: expires
  handler <- getCached _handler
  runHandler handler
  -- TODO: multiple choices

handleGetHeadNonExisting :: RestM ()
handleGetHeadNonExisting = do
  config <- ask
  -- TODO: Has if match? If so: 412

  -- Did this resource exist before?
  existed <- previouslyExisted config
  unless existed (stopWith NotFound404)

  movedPermanently config >>= moved MovedPermanently301
  movedTemporarily config >>= moved MovedTemporarily307
  stopWith Gone410
    where moved e = \case NotMoved    -> return ()
                          MovedTo url -> setHeader' "location" url >> stopWith e

handlePutPostPatchNonExisting :: RestM ()
handlePutPostPatchNonExisting = acceptResource -- FIXME

handlePutPostPatchExisting :: RestM ()
handlePutPostPatchExisting = do
  config <- ask
  method <- getCached _method
  -- TODO: cond
  when (method == PUT) $ do
    conflict <- isConflict config
    when conflict (stopWith Conflict409)

  acceptResource


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
       Failed                   -> status' badRequest400
       Succeeded                -> status' noContent204
       SucceededWithUrl url     -> setHeader' "location" url >> status' seeOther303
       SucceededWithContent t c -> setContentTypeHeader t >> (raw' . LE.encodeUtf8) c

setContentTypeHeader :: MediaType -> RestM ()
setContentTypeHeader = setHeader' "content-type" . LE.decodeUtf8 . BS.fromStrict . renderHeader

getCached :: (RequestState -> Maybe b) -> RestM b
getCached accessor = liftM accessor get >>= \case
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
handleExcept ServiceUnavailable503   = status serviceUnavailable503
handleExcept NotImplemented501       = status notImplemented501
handleExcept (InternalServerError s) = text s >> status internalServerError500
