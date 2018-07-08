{-# LANGUAGE DeriveGeneric #-}

module DWWebhooks.Config
    ( Config (..)

    , loadConfig
    ) where

import Data.Text.Lazy (pack)
import Dhall (Interpret, input, auto)
import qualified DWWebhooks.GitHub as G (SecretToken)
import qualified DWWebhooks.Jenkins as J
    ( HostURL
    , APIUsername
    , APIKey
    )
import GHC.Generics (Generic)

data Config = Config
    { gitHubSecretToken :: G.SecretToken
    , jenkinsHostURL :: J.HostURL
    , jenkinsAPIUsername :: J.APIUsername
    , jenkinsAPIKey :: J.APIKey
    } deriving (Generic, Show)

instance Interpret Config

loadConfig :: FilePath -> IO Config
loadConfig path = input auto (pack path)
