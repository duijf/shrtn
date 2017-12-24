{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as C8ByteString
import qualified Data.ByteString.Lazy.Char8 as L8ByteString
import           Data.Default (def)
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.HttpAuth as Auth
import qualified Web.FormUrlEncoded as Form

import Data.Text (Text)
import Web.FormUrlEncoded (FromForm)

import qualified Shrtn.State as State

redirectPort, managementPort :: Int
redirectPort = 7000
managementPort = 7001

main :: IO ()
main = do
  state <- State.new def
  let
    app =
      Warp.runSettings
        (settings redirectPort)
        (shrtnApp state)
    admin =
      Warp.runSettings
        (settings managementPort)
        (basicAuth $ mngmntApp state)

  putStrLn $ ":: Binding to ports " ++ show redirectPort ++
             " and " ++ show managementPort
  foldr1 Async.race_ [app, admin, State.run state]

-- Warp

settings :: Int -> Warp.Settings
settings port
  = Warp.setPort port
  $ Warp.setHost "*"
  $ Warp.defaultSettings

shrtnApp :: State.Handle -> Wai.Application
shrtnApp state request respond =
  case Wai.requestMethod request of
    "GET" -> do
      let slug = Maybe.fromMaybe "" $ Maybe.listToMaybe $ Wai.pathInfo request
      dest <- State.atomically $ State.lookupDest state slug
      respond $
        case dest of
          Just d  -> redirectTo (Text.encodeUtf8 d)
          Nothing -> notFound
    _ -> respond methodUnsupported

mngmntApp :: State.Handle -> Wai.Application
mngmntApp state request respond = do
  case Wai.pathInfo request of
    [] -> case Wai.requestMethod request of
      "GET" -> do
        contents <- L8ByteString.readFile "index.html"
        respond $ Wai.responseLBS Http.status200 [] contents
      _ -> respond $ methodUnsupported
    ["aliases"] -> case Wai.requestMethod request of
      "GET" -> do
        allRedirects <- State.atomically $ State.listAll state
        respond $
          Wai.responseLBS
            Http.status200
            [(Http.hContentType, "application/json")]
            (Aeson.encode allRedirects)
      "POST" -> do
        body <- Wai.lazyRequestBody request
        case Form.urlDecodeAsForm body of
          Left _err -> do
            print body
            respond badRequest
          Right r@RandomRedirect{..} -> do
            State.atomically $ State.insertRandom state _redirectDest
            putStrLn $ ":: Created new redirect " ++ show r
            respond success
          Right r@CustomRedirect{..} -> do
            insertRes <- State.atomically $ State.insertIfNotExists state _redirectSlug _redirectDest
            case insertRes of
              State.InsertSuccess -> do
                putStrLn $ ":: Created new redirect " ++ show r
                respond success
              State.SlugAlreadyExists -> respond conflict
      _ -> respond methodUnsupported
    ["style.css"] -> case Wai.requestMethod request of
      "GET" -> do
        contents <- L8ByteString.readFile "style.css"
        respond $ Wai.responseLBS Http.status200 [] contents
      _ -> respond methodUnsupported
    _ -> respond notFound

basicAuth :: Wai.Middleware
basicAuth =
  let checkCreds u p = return $ u == "admin" && p == "admin"
      realm = "shrtn administration"
  in Auth.basicAuth checkCreds realm

data RedirectReq
  = CustomRedirect
    { _redirectSlug :: Text
    , _redirectDest :: Text
    }
  | RandomRedirect
    { _redirectDest :: Text
    }

instance Show RedirectReq where
  show CustomRedirect{..} =
    concatMap Text.unpack ["/",  _redirectSlug, " -> ", _redirectDest]
  show RandomRedirect{..} =
    concatMap Text.unpack ["/random -> ", _redirectDest]

instance FromForm RedirectReq where
  fromForm f
    | Either.isLeft custom = random
    | otherwise     = custom
    where
      custom =
        CustomRedirect
        <$> Form.parseUnique "slug" f
        <*> Form.parseUnique "dest" f
      random =
        RandomRedirect
        <$> Form.parseUnique "dest" f

-- Wai utils

success :: Wai.Response
success = Wai.responseLBS Http.status200 [] ""

badRequest :: Wai.Response
badRequest = Wai.responseLBS Http.status400 [] ""

notFound :: Wai.Response
notFound = Wai.responseLBS Http.status404 [] ""

methodUnsupported :: Wai.Response
methodUnsupported = Wai.responseLBS Http.status405 [] ""

conflict :: Wai.Response
conflict = Wai.responseLBS Http.status409 [] ""

redirectTo :: C8ByteString.ByteString -> Wai.Response
redirectTo dest =
  Wai.responseLBS
    Http.status302
    [(Http.hLocation, dest)]
    ""
