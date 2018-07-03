{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Yesod.Core
    (RenderRoute (..)
    , Yesod
    , mkYesod
    , parseRoutes
    , toWaiApp
    )

data DWWebhooks = DWWebhooks

mkYesod "DWWebhooks" [parseRoutes|
    / RootR GET
|]

instance Yesod DWWebhooks

getRootR :: Handler Text
getRootR = pure "Hello, world!"

main :: IO ()
main = run 3000 =<< toWaiApp DWWebhooks
