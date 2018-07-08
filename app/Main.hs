{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT, scottyT)
import System.Environment (getEnv)

import qualified DWWebhooks.Config as C (loadConfig)
import qualified DWWebhooks.AppData as A (AppData, mkAppData)
import qualified DWWebhooks.Routes as R (routes)

runApp :: A.AppData -> ScottyT Text (ReaderT A.AppData IO) () -> IO ()
runApp appData = scottyT 3000 (`runReaderT` appData)

main :: IO ()
main = do
    configPath <- getEnv "DW_WEBHOOKS_CONFIG_FILE"
    appData <- A.mkAppData <$> C.loadConfig configPath
    runApp appData R.routes
