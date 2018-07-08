{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DWWebhooks.Jenkins
    ( JobName (JobName)
    , ParamName (ParamName)
    , ParamValue (ParamValue)

    , HostURL (HostURL)
    , APIUsername (APIUsername)
    , APIKey (APIKey)

    , JenkinsException (JenkinsHttpException)

    , Master

    , mkMaster
    , buildDWJob
    ) where

import Data.Coerce (coerce)

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Dhall (Interpret)
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

newtype HostURL = HostURL Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype APIUsername = APIUsername Text
    deriving (Eq, Ord, Show, Generic, Interpret)

newtype APIKey = APIKey Text
    deriving (Eq, Ord, Show, Generic, Interpret)

mkMaster :: HostURL -> APIUsername -> APIKey -> Master
mkMaster url name key = Master (unpack $ coerce url) (coerce name) (coerce key)

newtype JobName = JobName Text
    deriving (Eq, Ord, Show)

newtype ParamName = ParamName Text
    deriving (Eq, Ord, Show)

newtype ParamValue = ParamValue Text
    deriving (Eq, Ord, Show)

type Params = [(ParamName, ParamValue)]

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
