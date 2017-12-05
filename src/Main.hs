{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy.Char8 as L8ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Control.Concurrent.STM (STM, TVar)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

main :: IO ()
main = do
  state <- STM.atomically openState
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

shrtnApp :: TVar ShrtnState -> Wai.Application
shrtnApp _state request respond =
  case Wai.requestMethod request of
    "GET" -> respond $
      Wai.responseLBS
        Http.status200
        []
        "Main app"
    "POST" -> respond methodUnsupported
    _ -> respond methodUnsupported

mngmntApp :: TVar ShrtnState -> Wai.Application
mngmntApp state _request respond = do
  statePrint <- fmap show $ STM.atomically (STM.readTVar state)

  respond $
    Wai.responseLBS
      Http.status200
      []
      (L8ByteString.pack statePrint)

-- State

type Slug = Text
type Dest = Text
type ShrtnState = HashMap Slug Dest

openState :: STM (TVar ShrtnState)
openState =
  let
    empty = HashMap.empty :: ShrtnState
    withDefault = HashMap.insert "" "https://svsticky.nl" empty
  in
    STM.newTVar withDefault

-- Wai utils

methodUnsupported :: Wai.Response
methodUnsupported = Wai.responseLBS Http.status405 [] ""
