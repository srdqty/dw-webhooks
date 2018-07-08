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
    , SignatureValidationResult (Invalid, Valid)

    , validateGitHubSignature
    ) where

import Control.Monad.IO.Class (MonadIO)
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
import Data.Bool (bool)
import Data.ByteArray (ByteArrayAccess, constEq)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Coerce (coerce)
import qualified Data.Text as TS (Text, drop)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict)
import GHC.Generics (Generic)
import Web.Scotty.Trans
    ( ActionT
    , ScottyError
    , header
    )

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

newtype BranchName = BranchName TS.Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype CommitSha = CommitSha TS.Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype RepoOwner = RepoOwner TS.Text
    deriving (Eq, Ord, Show, ToJSON, FromJSON)

newtype RepoName = RepoName TS.Text
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

data SignatureValidationResult = Invalid | Valid deriving (Eq, Ord, Show)

validateGitHubSignature :: (MonadIO m, ScottyError e)
                        => SecretToken
                        -> Payload
                        -> ActionT e m SignatureValidationResult
validateGitHubSignature secret payload = do
    maybeSignature <- getSignature
    case maybeSignature of
        Nothing -> return Invalid
        Just sig -> return (bool Invalid Valid (isSignatureValid sig))
    where
        getSignature :: (MonadIO m, ScottyError e)
                     => ActionT e m (Maybe ProvidedSignature)
        getSignature = do
            signature <- header "X-Hub-Signature"
            -- drop the sha1= prefix
            return $
                coerce . encodeUtf8 . TS.drop 5 . TL.toStrict <$> signature

        calcSignature :: SecretToken -> Payload -> HMAC SHA1
        calcSignature = hmac

        isSignatureValid :: ProvidedSignature -> Bool
        isSignatureValid providedSignature =
            constEq providedSignature digest where
                digest =
                    pack $ show $ hmacGetDigest $ calcSignature secret payload
