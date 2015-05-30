{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Scotty.Rest.Types
  (
    RestM
  -- * Callbacks result types
  , Authorized(..)
  , DeleteResult(..)
  , ETag(..)
  , Moved(..)
  , ProcessingResult(..)
  , Representation (..)
  , RestConfig(..)
  , RestException(..)
  , Url
  -- * Re-exports
  , MediaType
  , Language
  , StdMethod(..)
  ) where

import BasePrelude hiding (Handler)

import           Control.Monad.IO.Class (MonadIO)
import           Data.Default.Class     (Default (..), def)
import qualified Data.Text.Lazy         as TL
import           Data.Time.Clock        (UTCTime)
import           Network.HTTP.Media     (Language, MediaType)
import           Network.HTTP.Types     (StdMethod (..))
import           Web.Scotty.Trans       hiding (get)

type RestM m = ActionT RestException m

--type Handler = ActionT RestException IO

type Url = TL.Text

data Moved = NotMoved
           | MovedTemporarily Url
           | MovedPermanently Url

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

-- | The callbacks that control a handler's behaviour.
-- 'Scotty.Rest.defaultConfig' returns a 'RestConfig' with default values. For
-- typical handlers, you only need to override a few of these callbacks.
data RestConfig m = RestConfig
  { allowedMethods       :: m [StdMethod]
  , resourceExists       :: m Bool
  , previouslyExisted    :: m Bool
  , isConflict           :: m Bool
  , contentTypesAccepted :: m [(MediaType, m ProcessingResult)]
  , contentTypesProvided :: m [(MediaType, m ())]
  , languagesProvided    :: m (Maybe [Language])
  , charsetsProvided     :: m (Maybe [TL.Text])
  , deleteResource       :: m DeleteResult
  , optionsHandler       :: m (Maybe (MediaType, m ()))
  , generateEtag         :: m (Maybe ETag)
  , expires              :: m (Maybe UTCTime)
  , lastModified         :: m (Maybe UTCTime)
  -- | If 'True', the request is considered malformed and a /400 Bad Request/ is returned.
  , malformedRequest     :: m Bool
  , isAuthorized         :: m Authorized
  -- | If 'True', access to this resource is forbidden, and /403 Forbidden/ is returned.
  --
  -- Default: 'False'.
  , forbidden            :: m Bool
  , serviceAvailable     :: m Bool
  , allowMissingPost     :: m Bool
  , multipleChoices      :: m Representation
  -- | Default: 'NotMoved'
  , resourceMoved        :: m Moved
  -- | Returns a list of header names that should be included in a given response's /Vary/ header.
  -- The standard content negotiation headers (/Accept/, /Accept-Encoding/, /Accept-Charset/,
  -- /Accept-Language/) do not need to be specified here as they will be added automatically when
  -- e.g.  several content types are provided.
  --
  -- Default: @[]@
  , variances            :: m [TL.Text]
  }

instance (MonadIO m) => Default (RestConfig m) where
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
                  , malformedRequest     = return False
                  , isAuthorized         = return Authorized
                  , forbidden            = return False
                  , serviceAvailable     = return True
                  , allowMissingPost     = return True
                  , multipleChoices      = return UniqueRepresentation
                  , resourceMoved        = return NotMoved
                  , variances            = return []
                  }

data RestException = MovedPermanently301
                   | NotModified304
                   | MovedTemporarily307
                   | BadRequest400
                   | Unauthorized401
                   | Forbidden403
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
