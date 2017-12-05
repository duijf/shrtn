{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

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

shrtnApp :: ShrtnState -> Wai.Application
shrtnApp _state _request respond =
  respond $
    Wai.responseLBS
      Http.status200
      []
      "Main app"

mngmntApp :: ShrtnState -> Wai.Application
mngmntApp _state _request respond =
  respond $
    Wai.responseLBS
      Http.status200
      []
      "Management app"

-- State

type Slug = Text
type Dest = Text
type ShrtnState = HashMap Slug Dest

openState :: IO ShrtnState
openState =
  let
    path = "shrtn.state"
    empty = HashMap.empty :: ShrtnState
  in
    pure empty
