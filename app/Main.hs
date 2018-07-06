{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad (unless)
import qualified Data.ByteString as BS (ByteString, drop)
import qualified Data.ByteString.Lazy as BL (fromChunks, toStrict)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List (consume)
import Data.Coerce (coerce)
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Yesod.Core
    ( RenderRoute (..)
    , Yesod
    , mkYesod
    , parseRoutes
    , toWaiApp
    , lookupHeader
    , sendResponseStatus
    , notFound
    , rawRequestBody
    , MonadHandler
    )
import Network.HTTP.Types.Status (status201)
import DWWebhooks.GitHub

data DWWebhooks = DWWebhooks

mkYesod "DWWebhooks" [parseRoutes|
    / RootR GET POST
|]

instance Yesod DWWebhooks

getRootR :: Handler Text
getRootR = pure "Hello, world!"

requestBody :: MonadHandler m => m BS.ByteString
requestBody = fmap
    (BL.toStrict . BL.fromChunks)
    (runConduit $ rawRequestBody .| consume)

validateGitHubSignature :: BS.ByteString -> Handler ()
validateGitHubSignature payload = do
    signature <- getSignature
    unless (isSignatureValid secret (coerce payload) signature)
        notFound
    where
        secret :: SecretToken
        secret = SecretToken "secret"

        getSignature :: Handler ProvidedSignature
        getSignature = do
            maybeSignature <- lookupHeader "X-Hub-Signature"
            case maybeSignature of
                Nothing -> notFound
                -- drop the sha1= prefix
                Just signature -> return (coerce (BS.drop 5 signature))


postRootR :: Handler ()
postRootR = do
    payload <- requestBody
    validateGitHubSignature payload
    sendResponseStatus status201 ()

main :: IO ()
main = run 3000 =<< toWaiApp DWWebhooks
