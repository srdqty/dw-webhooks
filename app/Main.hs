{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Coerce (coerce)
import Data.Text (Text, unpack)
import DWWebhooks.Config
import Jenkins.Rest

buildJob :: Text -> [(Text,Maybe Text)] -> Method 'Complete f
buildJob jobName ps =
    view "data-warehouse" -/- job jobName -/- "buildWithParameters" -?- query ps

main :: IO ()
main = do
    (Config _ _ jhu jau jak) <- loadConfig "./config.dhall"
    let master = Master (unpack $ coerce jhu) (coerce jau) (coerce jak)

    x <- run master $ post_ $
        buildJob "typecheck-pdt-config-files" [("branch", Just "master")]
    print x
