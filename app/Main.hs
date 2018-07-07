{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT, lift)
import Control.Monad.Reader.Class (MonadReader (ask))
import qualified Data.ByteString as BS (ByteString, drop)
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Coerce (coerce)
import qualified Data.Text as T (Text, drop)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL (Text, toStrict)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status (status201, status404)
import DWWebhooks.GitHub
import Web.Scotty.Trans
    ( ActionT
    , ScottyError
    , scottyT
    , body
    , get
    , header
    , post
    , status
    , text
    )

newtype Config = Config SecretToken

config :: Config
config = Config (SecretToken "secret")

postAction :: (MonadIO m, MonadReader Config m)
          => ActionT TL.Text m ()
postAction = do
    (Config secret) <- lift ask
    b <- BL.toStrict <$> body
    maybeSignature <- getSignature
    isValid <- case maybeSignature of
        Nothing -> return False
        Just x -> return (isSignatureValid secret (coerce b) x)
    status (if isValid then status201 else status404)
    where
        getSignature = do
            signature <- header "X-Hub-Signature"
            -- drop the sha1= prefix
            return (coerce . encodeUtf8 . T.drop 5 . TL.toStrict <$> signature)

main :: IO ()
main = scottyT 3000 (`runReaderT` config) $ do
    get "/" $ text "Hello, world!"
    post "/" postAction
