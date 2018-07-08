{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DWWebhooks.Routes where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Web.Scotty.Trans
    ( ScottyError
    , ScottyT
    , get
    , post
    , text
    )

import qualified DWWebhooks.Actions.TypeCheckPDTConfigFiles as TCPCF (action)
import  DWWebhooks.AppData (AppData)

routes :: (MonadIO m, MonadReader AppData m, ScottyError e) => ScottyT e m ()
routes = do
    get "/" $ text "Hello, world!"
    post "/hooks/type-check-pdt-config-files" TCPCF.action
