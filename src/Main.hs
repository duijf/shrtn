{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import GHC.Generics (Generic)
import Servant

main :: IO ()
main = do
  Warp.runSettings
    shrtnSettings
    shrtnApp

shrtnSettings :: Warp.Settings
shrtnSettings
  = Warp.setPort 7000
  $ Warp.setHost "*"
  $ Warp.defaultSettings

shrtnApp :: Wai.Application
shrtnApp _request respond =
  respond $
    Wai.responseLBS
      HTTP.status200
      []
      "Things are working pretty nicely."

mngmntApp :: Wai.Application
mngmntApp =
  Servant.serve
    (Proxy :: Proxy Mngmnt)
    mngmnt

type Mngmnt
  = "shorten"
    :> ReqBody '[JSON] CreateReq
    :> Post '[JSON] NoContent

data CreateReq = CreateReq
  { url :: T.Text
  , alias :: Maybe T.Text
  } deriving (Generic, Aeson.FromJSON)

mngmnt :: Server Mngmnt
mngmnt = createRoute

createRoute :: CreateReq -> Handler NoContent
createRoute _request = pure NoContent
