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
import Web.Scotty.Trans
import Network.HTTP.Types.Method (StdMethod(..))
import Network.HTTP.Types (parseMethod)
import Network.HTTP.Types.Status
import Network.HTTP.Media (MediaType, mapAccept, mapContent)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import Network.Wai (requestMethod)
import Data.String (fromString)
import Control.Monad.State

type Url = TL.Text
type Challenge = TL.Text

data Moved = NotMoved | MovedTo Url
data ProcessingResult = ProcessingSucceeded
                      | ProcessingSucceededWithUrl Url
                      | ProcessingFailed
data Authorized = Authorized | NotAuthorized Challenge

rest :: (MonadIO m) => RoutePattern -> RestConfig (ScottyRestM m) -> ScottyT RestException m ()
rest pattern handler = matchAny pattern (restHandlerStart handler `rescue` handleExcept)

data RestConfig m = RestConfig
  { allowedMethods       :: m [StdMethod]
  , resourceExists       :: m Bool
  , previouslyExisted    :: m Bool
  , contentTypesAccepted :: m [(MediaType, m ProcessingResult)]
  , contentTypesProvided :: m [(MediaType, m ())]
  , optionsHandler       :: m (Maybe (m ()))
  , charSetsProvided     :: m (Maybe [TL.Text])
  , isAuthorized         :: m Authorized
  , serviceAvailable     :: m Bool
  , movedPermanently     :: m Moved
  , movedTemporarily     :: m Moved
  }

defaultConfig :: (MonadIO m) => RestConfig (ScottyRestM m)
defaultConfig = RestConfig
  { allowedMethods       = return [GET, HEAD, OPTIONS]
  , resourceExists       = return True
  , previouslyExisted    = return False
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
                   | Unauthorized401
                   | NotFound404
                   | NotAcceptable406
                   | Gone410
                   | NotImplemented501
                   | ServiceUnavailable503
                   | MethodNotAllowed405
                   | InternalServerError TL.Text
                   deriving (Show, Eq)

instance ScottyError RestException where
  stringError = InternalServerError . TL.pack
  showError = fromString . show

type ScottyRestM m = ActionT RestException m
type RestHandler m = ScottyRestM m ()

restHandlerStart :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
restHandlerStart config = do
  -- Is our service available?
  available <- serviceAvailable config
  unless available $ raise ServiceUnavailable503

  ---- Is the method known?
  method <- either (\_ -> raise NotImplemented501) return . parseMethod . requestMethod =<< request

  -- TODO: Is the URI too long?

  -- Is the method allowed?
  allowed <- allowedMethods config
  when (method `notElem` allowed) $ do
    setAllowHeader config
    raise MethodNotAllowed405

  -- TODO: Is the request malformed?

  -- Is the client authorized?
  isAuthorized config >>= \case
       Authorized                -> return ()
       (NotAuthorized challenge) -> setHeader "WWW-Authenticate" challenge >> raise Unauthorized401

  -- TODO: Is the client forbidden to access this resource?
  -- TODO: Are the content headers valid?
  -- TODO: Is the entity length valid?

  if method == OPTIONS
     then handleOptions config
     else contentNegotiation method config

setAllowHeader :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
setAllowHeader config =
  setHeader "allow" . TL.intercalate ", " . map (TL.pack . show) =<< allowedMethods config

handleOptions :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
handleOptions config = fromMaybe (setAllowHeader config) =<< optionsHandler config

contentNegotiation :: (MonadIO m) => StdMethod -> RestConfig (ScottyRestM m) -> ScottyRestM m ()
contentNegotiation method config = do
  -- If there is an `Accept` header, raise a NotAcceptable406 exception if we
  -- cannot provide that type:
  accept <- return . E.encodeUtf8 . TL.toStrict . fromMaybe "*/*" =<< header "accept"
  provided <- contentTypesProvided config
  handler <- maybe (raise NotAcceptable406) return (mapAccept provided accept)

  -- TODO: If there is an `Accept-Language` header, check that we provide that
  -- language. If not → 406.
  -- TODO: If there is an `Accept-Charset` header, check that we provide that
  -- char set. If not → 406.
  -- TODO: Variances

  checkResourceExists method handler config

checkResourceExists :: (MonadIO m) => StdMethod -> RestHandler m -> RestConfig (ScottyRestM m) -> ScottyRestM m ()
checkResourceExists method handler config = do
  exists <- resourceExists config
  if | method `elem` [GET, HEAD]        -> if exists
                                              then handleGetHeadExisting handler config
                                              else handleGetHeadNonExisting handler config
     | method `elem` [PUT, POST, PATCH] -> if exists
                                              then handlePutPostPatchExisting config
                                              else handlePutPostPatchNonExisting config

handleGetHeadExisting :: (MonadIO m) => RestHandler m -> RestConfig (ScottyRestM m) -> ScottyRestM m ()
handleGetHeadExisting handler _callBacks = do
  -- TODO: generate etag
  -- TODO: last modified
  -- TODO: expires
  handler -- TODO: allow streaming a response
  status ok200
  -- TODO: multiple choices

handleGetHeadNonExisting :: (MonadIO m) => RestHandler m -> RestConfig (ScottyRestM m) -> ScottyRestM m ()
handleGetHeadNonExisting _handler config = do
  -- TODO: Has if match? If so: 412

  -- Did this resource exist before?
  existed <- previouslyExisted config
  unless existed (raise NotFound404)

  movedPermanently config >>= moved MovedPermanently301
  movedTemporarily config >>= moved MovedTemporarily307
  raise Gone410
    where moved e = \case NotMoved    -> return ()
                          MovedTo url -> setHeader "location" url >> raise e

handlePutPostPatchNonExisting :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
handlePutPostPatchNonExisting config = handlePutPostPatch config -- FIXME

handlePutPostPatchExisting :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
handlePutPostPatchExisting config = handlePutPostPatch config -- FIXME

handlePutPostPatch :: (MonadIO m) => RestConfig (ScottyRestM m) -> ScottyRestM m ()
handlePutPostPatch config = do
  contentType <- liftM (encodeUtf8 . TL.toStrict . fromMaybe undefined) (header "content-type")
  handlers <- contentTypesAccepted config
  text (TL.pack . show $ contentType)
  result <- fromMaybe (raise NotAcceptable406) (mapContent handlers contentType)
  case result of
       ProcessingFailed               -> status badRequest400
       ProcessingSucceeded            -> status noContent204
       ProcessingSucceededWithUrl url -> setHeader "location" url >> status seeOther303

handleExcept :: (Monad m) => RestException -> ScottyRestM m ()
handleExcept MovedPermanently301     = status movedPermanently301
handleExcept MovedTemporarily307     = status temporaryRedirect307
handleExcept Unauthorized401         = status unauthorized401
handleExcept NotFound404             = status notFound404
handleExcept MethodNotAllowed405     = status methodNotAllowed405
handleExcept NotAcceptable406        = status notAcceptable406
handleExcept Gone410                 = status gone410
handleExcept ServiceUnavailable503   = status serviceUnavailable503
handleExcept NotImplemented501       = status notImplemented501
handleExcept (InternalServerError s) = text s >> status internalServerError500
