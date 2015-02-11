{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}

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
  -- * Lenses for request state's fields
  , method'
  , handler'
  , newResource'
  , eTag'
  , lastModified'
  -- * Re-exports
  , MediaType
  , StdMethod(..)
  , UTCTime
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, ReaderT)
import           Control.Monad.State (MonadState, StateT)
import           Data.Default.Class (Default(..), def)
import           Data.String (fromString)
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (UTCTime)
import           Lens.Family2
import           Lens.Family2.TH (makeLensesBy)
import           Network.HTTP.Media (MediaType)
import           Network.HTTP.Types (StdMethod(..))
import           Web.Scotty.Trans

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
