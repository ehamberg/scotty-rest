{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Web.Scotty.Rest.Types
  (
    RestM(..)
  -- * Callbacks result types
  , Moved(..)
  , ProcessingResult(..)
  , DeleteResult(..)
  , Authorized(..)
  , ETag(..)
  -- * Internal data types
  , RestConfig(..)
  , RestException(..)
  , Handler
  , Url
  , emptyHanderState
  -- * Lenses for request state's fields
  , config'
  , method'
  , handler'
  , newResource'
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
  , StdMethod(..)
  , UTCTime
  ) where

import           Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, putMVar, readMVar)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, ask)
import           Data.Default.Class      (Default (..), def)
import           Data.String             (fromString)
import qualified Data.Text.Lazy          as TL
import           Data.Time.Clock         (UTCTime)
import           Lens.Family2
import           Lens.Family2.TH         (makeLensesBy)
import           Network.HTTP.Media      (MediaType)
import           Network.HTTP.Types      (StdMethod (..))
import           Web.Scotty.Trans

newtype RestM a = RestM
  { runRestM :: ReaderT HandlerState (ActionT RestException IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerState)

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

emptyHanderState :: RestConfig -> ActionT RestException IO HandlerState
emptyHanderState config = do
  method      <- liftIO newEmptyMVar
  handler     <- liftIO newEmptyMVar
  newResource <- liftIO newEmptyMVar
  tag         <- liftIO newEmptyMVar
  expiry      <- liftIO newEmptyMVar
  modified    <- liftIO newEmptyMVar
  available   <- liftIO newEmptyMVar
  now         <- liftIO newEmptyMVar
  return (HandlerState config method handler newResource tag expiry modified available now)

data HandlerState = HandlerState
  {
    _config       :: !RestConfig
  , _method       :: !(MVar StdMethod)
  , _handler      :: !(MVar (Handler ()))
  , _newResource  :: !(MVar Bool)
  , _eTag         :: !(MVar (Maybe ETag))    -- ETag, if computed
  , _expires      :: !(MVar (Maybe UTCTime)) -- Expiry time, if computed
  , _lastModified :: !(MVar (Maybe UTCTime)) -- Last modified, if computed
  ,_isAvailable   :: !(MVar Bool)
  ,_now           :: !(MVar UTCTime)
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
  , expires              :: RestM (Maybe UTCTime)
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
                  , expires              = return Nothing
                  , lastModified         = return Nothing
                  , isAuthorized         = return Authorized
                  , serviceAvailable     = return True
                  , allowMissingPost     = return True
                  , multipleChoices      = return False
                  , movedPermanently     = return NotMoved
                  , movedTemporarily     = return NotMoved
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

store :: Lens' HandlerState (MVar a) -> a -> RestM ()
store field value = do
  state <- ask
  liftIO $ putMVar (view field state) value

retrieve :: Lens' HandlerState a -> RestM a
retrieve field = do
  state <- ask
  return (view field state)

computeOnce :: Lens' HandlerState (MVar a) -> RestM a -> RestM a
computeOnce field computation = do
  state <- ask
  let var = view field state
  empty <- liftIO $ isEmptyMVar var
  when empty (computation >>= liftIO . putMVar var)
  liftIO $ readMVar var

cached :: Lens' HandlerState (MVar a) -> RestM a
cached field = do
  state <- ask
  liftIO $ readMVar (view field state)

fromConfig :: (RestConfig -> RestM a) -> RestM a
fromConfig field = retrieve config' >>= field
