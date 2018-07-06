{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module DWWebhooks.GitHub
    ( PullRequestAction
        ( Assigned
        , Unassigned
        , ReviewRequested
        , ReviewRequestRemoved
        , Labeled
        , Unlabeled
        , Opened
        , Edited
        , Closed
        , Reopened
        )
    , BranchName (BranchName)
    , CommitSha (CommitSha)
    , RepoOwner (RepoOwner)
    , RepoName (RepoName)
    , PullRequestEvent (PullRequestEvent)

    , SecretToken (SecretToken)
    , Payload (Payload)
    , ProvidedSignature (ProvidedSignature)

    , isSignatureValid
    ) where

import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmacGetDigest, hmac)
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON, toEncoding)

    , camelTo2
    , constructorTagModifier
    , defaultOptions
    , genericParseJSON
    , genericToEncoding
    , genericToJSON
    , withObject

    , (.:)
    )
import Data.ByteArray (ByteArrayAccess, constEq)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import GHC.Generics (Generic)

data PullRequestAction
    = Assigned
    | Unassigned
    | ReviewRequested
    | ReviewRequestRemoved
    | Labeled
    | Unlabeled
    | Opened
    | Edited
    | Closed
    | Reopened
    deriving (Eq, Ord, Show, Generic)

instance ToJSON PullRequestAction where
    toJSON = genericToJSON defaultOptions
        { constructorTagModifier = camelTo2 '_' }
    toEncoding = genericToEncoding defaultOptions
        { constructorTagModifier = camelTo2 '_' }

instance FromJSON PullRequestAction where
    parseJSON = genericParseJSON defaultOptions
        { constructorTagModifier = camelTo2 '_' }

newtype BranchName = BranchName Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype CommitSha = CommitSha Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype RepoOwner = RepoOwner Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype RepoName = RepoName Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

data PullRequestEvent = PullRequestEvent
    PullRequestAction
    BranchName
    CommitSha
    RepoOwner
    RepoName
    deriving (Eq, Ord, Show)

instance FromJSON PullRequestEvent where
    parseJSON = withObject "PullRequestEvent" $ \o -> do
        action <- o .: "action"
        pr <- o .: "pull_request"
        hd <- pr .: "head"
        branch <- hd .: "ref"
        sha <- hd .: "sha"
        repo <- hd .: "repo"
        repoName <- repo .: "name"
        owner <- repo .: "owner"
        ownerName <- owner .: "login"
        return (PullRequestEvent action branch sha ownerName repoName)

newtype SecretToken = SecretToken ByteString
    deriving (Eq, Ord, Show, ByteArrayAccess)

newtype Payload = Payload ByteString
    deriving (Eq, Ord, Show, ByteArrayAccess)

newtype ProvidedSignature = ProvidedSignature ByteString
    deriving (Eq, Ord, Show, ByteArrayAccess)

calcSignature :: SecretToken -> Payload -> HMAC SHA1
calcSignature = hmac

isSignatureValid :: SecretToken -> Payload -> ProvidedSignature -> Bool
isSignatureValid secret payload providedSignature =
    constEq providedSignature digest where
        digest = pack $ show $ hmacGetDigest $ calcSignature secret payload
