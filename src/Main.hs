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
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.Acid as Acid
import qualified Data.Acid.Advanced as Acid
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import qualified Data.SafeCopy as SC

import Data.Maybe

import Control.Monad.State (State)
import Data.Acid (AcidState(..))
import Data.Acid.Advanced (IsAcidic(..), Event(QueryEvent, UpdateEvent))
import Data.Acid.Core (Method(..))
import Data.SafeCopy (SafeCopy)
import GHC.Generics (Generic)

import Data.Typeable
import Servant

main :: IO ()
main = do
  state <- openState

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

shrtnApp :: AcidState ST -> Wai.Application
shrtnApp _state _request respond =
  respond $
    Wai.responseLBS
      HTTP.status200
      []
      "Things are working pretty nicely."

mngmntApp :: AcidState ST -> Wai.Application
mngmntApp state =
  Servant.serve
    (Proxy :: Proxy Mngmnt)
    (mngmnt state)

-- State

type Key = T.Text
type Value = T.Text
data ST = ST !(HM.HashMap Key Value)
  deriving (Typeable)

instance SafeCopy ST where
  putCopy state = SC.contain $ SC.safePut state
  getCopy = SC.contain $ SC.safeGet

insertKey :: Key -> Value -> Acid.Update ST ()
insertKey key value = do
  ST current <- State.get
  State.put $ ST $ HM.insert key value current

lookupKey :: Key -> Acid.Query ST (Maybe Value)
lookupKey key = do
  ST current <- Reader.ask
  return $ HM.lookup key current

data InsertKey = InsertKey Key Value
data LookupKey = LookupKey Key

deriving instance Typeable InsertKey
instance SafeCopy InsertKey where
  putCopy (InsertKey key value) = SC.contain $ SC.safePut key >> SC.safePut value
  getCopy = SC.contain $ InsertKey <$> SC.safeGet <*> SC.safeGet

instance Method InsertKey where
    type MethodResult InsertKey = ()
    type MethodState InsertKey = ST
instance Acid.UpdateEvent InsertKey

deriving instance Typeable LookupKey
instance SafeCopy LookupKey where
    putCopy (LookupKey key) = SC.contain $ SC.safePut key
    getCopy = SC.contain $ LookupKey <$> SC.safeGet
instance Method LookupKey where
    type MethodResult LookupKey = Maybe Value
    type MethodState LookupKey = ST
instance Acid.QueryEvent LookupKey

instance IsAcidic ST where
  acidEvents = [ UpdateEvent (\(InsertKey key value) -> insertKey key value)
               , QueryEvent (\(LookupKey key) -> lookupKey key)
               ]

openState :: IO (AcidState ST)
openState =
  let
    path = "shrtn.state"
    empty = ST $ HM.empty
  in
    Acid.openLocalStateFrom
      path
      empty

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

mngmnt :: AcidState ST -> Server Mngmnt
mngmnt state = (createRoute state) :<|> (listRoute state)

listRoute :: AcidState ST -> Handler Aliases
listRoute state = do
  new <- Acid.query' state (LookupKey "blah")
  return $ Aliases { dest = "blah", short = fromJust new }

createRoute :: AcidState ST -> CreateReq -> Handler NoContent
createRoute state _request = do
  newState <- Acid.update' state (InsertKey "blah" "asdfasdf")
  pure NoContent
