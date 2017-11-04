{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.State as MState
import qualified Control.Monad.Reader as Reader
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Data.Maybe

import GHC.Generics (Generic)

import Data.Typeable
import Servant

import State

main :: IO ()
main = do
  state <- openState "shrtn"

  let
    app =
      Warp.runSettings
        (settings 7000)
        (shrtnApp state)
    admin =
      Warp.runSettings
        (settings 7001)
        (mngmntApp state)

  foldr1 Async.race_ [app, admin]

-- Warp

settings :: Int -> Warp.Settings
settings port
  = Warp.setPort port
  $ Warp.setHost "*"
  $ Warp.defaultSettings

shrtnApp :: AppST -> Wai.Application
shrtnApp _state _request respond =
  respond $
    Wai.responseLBS
      HTTP.status200
      []
      "Things are working pretty nicely."

mngmntApp :: AppST -> Wai.Application
mngmntApp state =
  Servant.serve
    (Proxy :: Proxy Mngmnt)
    (mngmnt state)


-- Servant

type Mngmnt
  = "shorten" :> ReqBody '[JSON] CreateReq :> Post '[JSON] NoContent
   :<|> "list" :> Get '[JSON] Aliases

data CreateReq = CreateReq
  { url :: T.Text
  , alias :: Maybe T.Text
  } deriving (Generic, Aeson.FromJSON)

data Aliases = Aliases
  { dest :: T.Text
  , short :: T.Text
  } deriving (Generic, Aeson.ToJSON)

mngmnt :: AppST -> Server Mngmnt
mngmnt state = (createRoute state) :<|> (listRoute state)

listRoute :: AppST -> Handler Aliases
listRoute state = do
  new <- lookup blah state
  return $ Aliases { dest = "blah", short = fromJust new }

createRoute :: AppST -> CreateReq -> Handler NoContent
createRoute state _request = do
  newState <- Acid.update' state (InsertKey "blah" "asdfasdf")
  pure NoContent
