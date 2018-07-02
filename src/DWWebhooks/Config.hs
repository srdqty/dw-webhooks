{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DWWebhooks.Config
    ( Config (..)

    , GithubAPIAccessToken (GithubAPIAccessToken)
    , GithubWebhookSecret (GithubWebhookSecret)
    , JenkinsHostURL (JenkinsHostURL)
    , JenkinsAPIUsername (JenkinsAPIUsername)
    , JenkinsAPIKey (JenkinsAPIKey)

    , loadConfig
    ) where

import Data.Text (Text)
import Data.Text.Lazy (pack)
import Dhall (Generic, Interpret, input, auto)

newtype GithubAPIAccessToken = GithubAPIAccessToken Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype GithubWebhookSecret = GithubWebhookSecret Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype JenkinsHostURL = JenkinsHostURL Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype JenkinsAPIUsername = JenkinsAPIUsername Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype JenkinsAPIKey = JenkinsAPIKey Text
    deriving (Eq, Ord, Show, Generic, Interpret)

data Config = Config
    { githubAPIAccessToken :: GithubAPIAccessToken
    , githubWebhookSecret :: GithubWebhookSecret
    , jenkinsHostURL :: JenkinsHostURL
    , jenkinsAPIUsername :: JenkinsAPIUsername
    , jenkinsAPIKey :: JenkinsAPIKey
    } deriving (Generic, Show)

instance Interpret Config

loadConfig :: FilePath -> IO Config
loadConfig path = input auto (pack path)
