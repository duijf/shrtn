{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Shrtn.Api
  ( Server
  , ShrtnApi
  , (:<|>)(..)
  , NewRedirect(..)
  , NoContent(..)
  , shrtnApiProxy
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Proxy (Proxy(..))
import           GHC.Generics (Generic)
import           Shrtn.State (State)
import           Servant (Server)
import           Servant.API (Get, Post, NoContent(..), ReqBody, JSON, (:<|>)(..), (:>))

import Shrtn.State (Slug, Dest)

data NewRedirect
  = NewRedirect
  { slug :: Maybe Slug -- Slugs are optional. If absent, we randomly generate
  , dest :: Dest
  } deriving (Eq, Show, Read, Generic)

instance FromJSON NewRedirect
instance ToJSON NewRedirect

type ShrtnApi
  =    "aliases" :> Get '[JSON] State
  :<|> "aliases" :> ReqBody '[JSON] NewRedirect :> Post '[JSON] NoContent

shrtnApiProxy :: Proxy ShrtnApi
shrtnApiProxy = Proxy
