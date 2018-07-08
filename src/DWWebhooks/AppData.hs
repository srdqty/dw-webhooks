module DWWebhooks.AppData
    ( AppData (..)
    , mkAppData
    ) where

import qualified DWWebhooks.Config as C
    ( Config
    , gitHubSecretToken
    , jenkinsHostURL
    , jenkinsAPIUsername
    , jenkinsAPIKey
    )
import qualified DWWebhooks.GitHub as GH (SecretToken)
import qualified DWWebhooks.Jenkins as J (Master, mkMaster)

data AppData = AppData
    { gitHubSecretToken :: GH.SecretToken
    , jenkinsMaster :: J.Master
    } deriving (Eq, Show)

mkAppData :: C.Config -> AppData
mkAppData config = AppData
    { gitHubSecretToken = C.gitHubSecretToken config
    , jenkinsMaster = jMaster
    }
    where
        jMaster = J.mkMaster
            (C.jenkinsHostURL config)
            (C.jenkinsAPIUsername config)
            (C.jenkinsAPIKey config)
