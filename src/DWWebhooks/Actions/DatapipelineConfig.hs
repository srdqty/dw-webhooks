{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module DWWebhooks.Actions.DatapipelineConfig where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (lift)
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Coerce (coerce)
import Network.HTTP.Types.Status (status201, status404)
import Web.Scotty.Trans
    ( ActionT
    , ScottyError
    , body
    , liftAndCatchIO
    , status
    )

import DWWebhooks.AppData
    ( AppData
    , gitHubSecretToken
    , jenkinsMaster
    )
import DWWebhooks.GitHub
    ( BranchName (BranchName)
    , Payload (Payload)
    , PullRequestAction (Opened)
    , PullRequestEvent (PullRequestEvent)
    , SignatureValidationResult (Invalid, Valid)
    , validateGitHubSignature
    )
import DWWebhooks.Jenkins
    ( JobName (JobName)
    , ParamName (ParamName)
    , ParamValue (ParamValue)
    , buildDWJob
    )

action :: (MonadIO m, MonadReader AppData m, ScottyError e)
       => ActionT e m ()
action = do
    appData <- lift ask
    let secret = gitHubSecretToken appData
    b <- body
    validation <- validateGitHubSignature secret (coerce $ BL.toStrict b)
    case validation of
        Invalid -> status status404
        Valid ->
            case decode b of
                Nothing -> status status404
                Just (PullRequestEvent Opened branchName _ _ _) -> do
                    let jMaster = jenkinsMaster appData
                    result <- liftAndCatchIO $
                        buildTypeCheckPDTConfigFiles jMaster branchName
                    status (either (const status404) (const status201) result)
                Just _ -> status status404
    where
        buildTypeCheckPDTConfigFiles jMaster branchName =
            buildDWJob jMaster
                (JobName "typecheck-pdt-config-files")
                [(ParamName "branch", coerce branchName)]
