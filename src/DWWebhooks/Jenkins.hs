{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module DWWebhooks.Jenkins
    ( JobName (JobName)
    , ParamName (ParamName)
    , ParamValue (ParamValue)

    , JenkinsHostURL (JenkinsHostURL)
    , JenkinsAPIUsername (JenkinsAPIUsername)
    , JenkinsAPIKey (JenkinsAPIKey)

    , JenkinsException (JenkinsHttpException)

    , mkMaster
    , buildDWJob
    ) where

import Data.Coerce (coerce)

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack)
import Jenkins.Rest
    ( Master (Master)
    , Method
    , Type (Complete)
    , JenkinsException (JenkinsHttpException)

    , job
    , query
    , run
    , post_
    , view

    , (-/-)
    , (-?-)
    )

import DWWebhooks.Config
    ( JenkinsHostURL (JenkinsHostURL)
    , JenkinsAPIUsername (JenkinsAPIUsername)
    , JenkinsAPIKey (JenkinsAPIKey)
    )

newtype JobName = JobName Text
    deriving (Eq, Ord, Show)

newtype ParamName = ParamName Text
    deriving (Eq, Ord, Show)

newtype ParamValue = ParamValue Text
    deriving (Eq, Ord, Show)

type Params = [(ParamName, ParamValue)]

mkMaster :: JenkinsHostURL -> JenkinsAPIUsername -> JenkinsAPIKey -> Master
mkMaster url name key = Master (unpack $ coerce url) (coerce name) (coerce key)

buildDWJob :: Master
           -> JobName
           -> Params
           -> IO (Either JenkinsException ByteString)
buildDWJob master jobName params = run master (post_ $ requestUrl jobName params)
    where
        requestUrl :: JobName -> Params -> Method 'Complete f
        requestUrl name ps
            = view "data-warehouse"
            -/- job (coerce name)
            -/- "buildWithParameters"
            -?- query (convertParams ps)

        convertParams :: Params -> [(Text, Maybe Text)]
        convertParams = fmap (\(x,y) -> (coerce x, Just (coerce y)))
