{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Web.Scotty.Rest.Types
  (
    RestM(..)
  -- * Callbacks result types
  , Authorized(..)
  , DeleteResult(..)
  , ETag(..)
  , Moved(..)
  , ProcessingResult(..)
  , Representation (..)
  -- * Internal data types
  , Handler
  , RestConfig(..)
  , RestException(..)
  , Url
  , emptyHandlerState
  -- * Lenses for request state's fields
  , config'
  , method'
  , handler'
  , language'
  , charset'
  , eTag'
  , expires'
  , lastModified'
  , isAvailable'
  , now'
  -- * Lens helpers
  , store
  , retrieve
  , computeOnce
  , cached
  , fromConfig
  -- * Re-exports
  , MediaType
  , Language
  , StdMethod(..)
  ) where

import Web.Scotty.Rest.Internal.CachedVar

import BasePrelude hiding (Handler)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask)
import           Data.Default.Class     (Default (..), def)
import qualified Data.Text.Lazy         as TL
import           Data.Time.Clock        (UTCTime)
import           Lens.Family2
import           Lens.Family2.TH        (makeLensesBy)
import           Network.HTTP.Media     (Language, MediaType)
import           Network.HTTP.Types     (StdMethod (..))
import           Web.Scotty.Trans

newtype RestM a = RestM
  { runRestM :: ReaderT HandlerState (ActionT RestException IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerState)

type Handler = ActionT RestException IO

type Url = TL.Text

data Moved = NotMoved | MovedTo Url
data ETag = Strong TL.Text
          | Weak TL.Text
data ProcessingResult = Succeeded
                      | SucceededWithContent MediaType TL.Text
                      | SucceededWithLocation Url
                      | Redirect Url
                      | Failed
data Representation = UniqueRepresentation
                    | MultipleRepresentations MediaType TL.Text
                    | MultipleWithPreferred MediaType TL.Text Url
data DeleteResult = NotDeleted
                  | DeleteCompleted
                  | DeleteEnacted -- accepted for processing, but the processing has not been completed (and may never be)
                  | DeletedWithResponse MediaType TL.Text
                  deriving Eq

-- | Response to 'isAuthorized' callback.
data Authorized = Authorized            -- ^ User is authenticated and authorized.
                | NotAuthorized TL.Text -- ^ User is not authorized. The given challenge will be
                                        --   sent as part of the /WWW-Authenticate/ header.

emptyHandlerState :: RestConfig -> ActionT RestException IO HandlerState
emptyHandlerState config = do
  method      <- liftIO newEmtpyCachedVar
  handler     <- liftIO newEmtpyCachedVar
  language    <- liftIO newEmtpyCachedVar
  charset     <- liftIO newEmtpyCachedVar
  tag         <- liftIO newEmtpyCachedVar
  expiry      <- liftIO newEmtpyCachedVar
  modified    <- liftIO newEmtpyCachedVar
  available   <- liftIO newEmtpyCachedVar
  now         <- liftIO newEmtpyCachedVar
  return (HandlerState config method handler language charset tag expiry modified available now)

data HandlerState = HandlerState
  {
    _config       :: !RestConfig
  , _method       :: !(CachedVar StdMethod)
  , _handler      :: !(CachedVar (MediaType,Handler ()))
  , _language     :: !(CachedVar (Maybe Language))
  , _charset      :: !(CachedVar (Maybe TL.Text))
  , _eTag         :: !(CachedVar (Maybe ETag))    -- ETag, if computed
  , _expires      :: !(CachedVar (Maybe UTCTime)) -- Expiry time, if computed
  , _lastModified :: !(CachedVar (Maybe UTCTime)) -- Last modified, if computed
  , _isAvailable  :: !(CachedVar Bool)
  , _now          :: !(CachedVar UTCTime)
  }

data RestConfig = RestConfig
  { allowedMethods       :: RestM [StdMethod]
  , resourceExists       :: RestM Bool
  , previouslyExisted    :: RestM Bool
  , isConflict           :: RestM Bool
  , contentTypesAccepted :: RestM [(MediaType, Handler ProcessingResult)]
  , contentTypesProvided :: RestM [(MediaType, Handler ())]
  , languagesProvided    :: RestM (Maybe [Language])
  , charsetsProvided     :: RestM (Maybe [TL.Text])
  , deleteResource       :: RestM DeleteResult
  , optionsHandler       :: RestM (Maybe (MediaType, Handler ()))
  , generateEtag         :: RestM (Maybe ETag)
  , expires              :: RestM (Maybe UTCTime)
  , lastModified         :: RestM (Maybe UTCTime)
  , isAuthorized         :: RestM Authorized
  , serviceAvailable     :: RestM Bool
  , allowMissingPost     :: RestM Bool
  , multipleChoices      :: RestM Representation
  , movedPermanently     :: RestM Moved
  , movedTemporarily     :: RestM Moved
  , variances            :: RestM [TL.Text]
  }

instance Default RestConfig where
 def = RestConfig { allowedMethods       = return [GET, HEAD, OPTIONS]
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
                  , isAuthorized         = return Authorized
                  , serviceAvailable     = return True
                  , allowMissingPost     = return True
                  , multipleChoices      = return UniqueRepresentation
                  , movedPermanently     = return NotMoved
                  , movedTemporarily     = return NotMoved
                  , variances            = return []
                  }

data RestException = MovedPermanently301
                   | NotModified304
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

$(makeLensesBy (\n -> Just (tail n ++ "'")) ''HandlerState)

store :: Lens' HandlerState (CachedVar a) -> a -> RestM ()
store field value = do
  state <- ask
  liftIO $ writeCachedVar (view field state) value

retrieve :: Lens' HandlerState a -> RestM a
retrieve field = do
  state <- ask
  return (view field state)

computeOnce :: Lens' HandlerState (CachedVar a) -> RestM a -> RestM a
computeOnce field computation = do
  state <- ask
  let ref = view field state
  contents <- liftIO $ readIORef ref
  maybe (computation >>= liftIO . writeAndReturnCachedVar ref) return contents

cached :: Lens' HandlerState (CachedVar a) -> RestM a
cached field = do
  state <- ask
  liftIO $ readCachedVar (view field state)

fromConfig :: (RestConfig -> RestM a) -> RestM a
fromConfig field = retrieve config' >>= field
