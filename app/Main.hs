{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT, lift)
import Control.Monad.Reader.Class (MonadReader (ask))
import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Coerce (coerce)
import qualified Data.Text.Lazy as TL (Text)
import Network.HTTP.Types.Status (status201, status404)
import DWWebhooks.GitHub
import Web.Scotty.Trans
    ( ActionT
    , scottyT
    , body
    , get
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
    validation <- validateGitHubSignature secret (coerce b)
    case validation of
        Invalid -> status status404
        Valid -> status status201

main :: IO ()
main = scottyT 3000 (`runReaderT` config) $ do
    get "/" $ text "Hello, world!"
    post "/" postAction
