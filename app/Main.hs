{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Yesod.Core
    ( RenderRoute (..)
    , Yesod
    , mkYesod
    , parseRoutes
    , toWaiApp
    , requireJsonBody
    , sendStatusJSON
    )
import Data.Aeson (Value)
import Network.HTTP.Types.Status (status201)

data DWWebhooks = DWWebhooks

mkYesod "DWWebhooks" [parseRoutes|
    / RootR GET POST
|]

instance Yesod DWWebhooks

getRootR :: Handler Text
getRootR = pure "Hello, world!"

postRootR :: Handler Text
postRootR = do
    v <- requireJsonBody :: Handler Value
    sendStatusJSON status201 v

main :: IO ()
main = run 3000 =<< toWaiApp DWWebhooks
