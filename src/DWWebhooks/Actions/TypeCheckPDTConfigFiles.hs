{-# LANGUAGE FlexibleContexts #-}

module DWWebhooks.Actions.TypeCheckPDTConfigFiles where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (lift)
import Control.Monad.Reader.Class (MonadReader (ask))
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Coerce (coerce)
import DWWebhooks.AppData (AppData, gitHubSecretToken)
import DWWebhooks.GitHub
    ( Payload (Payload)
    , SignatureValidationResult (Invalid, Valid)
    , validateGitHubSignature
    )
import Network.HTTP.Types.Status (status201, status404)
import Web.Scotty.Trans
    ( ActionT
    , ScottyError
    , body
    , status
    )

action :: (MonadIO m, MonadReader AppData m, ScottyError e)
       => ActionT e m ()
action = do
    secret <- gitHubSecretToken <$> lift ask
    b <- BL.toStrict <$> body
    validation <- validateGitHubSignature secret (coerce b)
    case validation of
        Invalid -> status status404
        Valid -> status status201
