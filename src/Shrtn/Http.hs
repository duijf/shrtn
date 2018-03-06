{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Shrtn.Http
  ( new
  , run
  , Config
  , Handle
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import           Data.Default (Default, def)
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HttpTypes
import qualified Network.Wai.Extended as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.HttpAuth as Auth

import qualified Shrtn.State as State
import qualified Shrtn.Views as Views
import           Shrtn.Api (ShrtnApi, shrtnApiProxy, Server, NewRedirect(..), NoContent(..), (:<|>)(..))

import Servant (serve)

data Config = Config
  { cRedirectPort :: Int
  , cManagementPort :: Int
  , cAuthEnabled :: Bool
  }

instance Default Config where
  def = Config
    { cRedirectPort = 7000
    , cManagementPort = 7001
    , cAuthEnabled = True
    }

data Handle = Handle
  { hConfig :: Config
  , hApp :: IO ()
  , hMngmt :: IO ()
  , hState :: State.Handle
  }

new :: State.Handle -> Config -> IO Handle
new state c@Config{..} =
  pure $ Handle
    { hConfig = c
    , hApp = runWarp cRedirectPort (shrtnApp state)
    , hMngmt = runWarp cManagementPort (basicAuth c $ mngmtApp state)
    , hState = state
    }

runWarp :: Int -> Wai.Application -> IO ()
runWarp port waiApp =
  Warp.runSettings
    ( Warp.setPort port
    $ Warp.setHost "*"
    $ Warp.defaultSettings)
    waiApp

run :: Handle -> IO ()
run Handle{..} = Async.race_ hApp hMngmt

shrtnApp :: State.Handle -> Wai.Application
shrtnApp state request respond =
  case Wai.requestMethod request of
    "GET" -> do
      let slug = Maybe.fromMaybe "" $ Maybe.listToMaybe $ Wai.pathInfo request
      dest <- State.atomically $ State.lookupDest state slug
      respond $
        case dest of
          Just d  -> Wai.redirectTo (Text.encodeUtf8 d)
          Nothing -> Wai.notFound
    _ -> respond Wai.methodUnsupported

mngmtApi :: State.Handle -> Server ShrtnApi
mngmtApi handle = getAliases :<|> postAliases
  where
    getAliases = liftIO $ State.atomically $ State.listAll handle
    postAliases NewRedirect{..} =
      liftIO $ State.atomically $
        case slug of
          Just s -> do
            res <- State.insertIfNotExists handle s dest
            case res of
              State.InsertSuccess -> pure NoContent
              State.SlugAlreadyExists -> pure NoContent
          Nothing -> do
            _ <- State.insertRandom handle dest
            pure NoContent

mngmtApp :: State.Handle -> Wai.Application
mngmtApp state request respond = do
  case Wai.pathInfo request of
    [] -> case Wai.requestMethod request of
      "GET" -> do
        respond $ Wai.responseLBS HttpTypes.status200 [] Views.mngmtView
      _ -> respond $ Wai.methodUnsupported
    ["aliases"] -> (serve shrtnApiProxy (mngmtApi state)) request respond
    ["style.css"] -> case Wai.requestMethod request of
      "GET" -> do
        contents <- BSLC8.readFile "style.css"
        respond $ Wai.responseLBS HttpTypes.status200 [] contents
      _ -> respond Wai.methodUnsupported
    _ -> respond Wai.notFound

basicAuth :: Config -> Wai.Middleware
basicAuth Config{..}
  | cAuthEnabled == False = id
  | otherwise = let checkCreds u p = return $ u == "admin" && p == "admin"
                    realm = "shrtn administration"
                in Auth.basicAuth checkCreds realm
