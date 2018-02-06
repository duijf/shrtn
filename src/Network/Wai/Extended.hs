{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Extended
  ( module Wai
  , success
  , badRequest
  , notFound
  , methodUnsupported
  , conflict
  , redirectTo
  ) where

import qualified Data.ByteString.Char8 as BSC8
import qualified Network.HTTP.Types as HttpTypes
import           Network.Wai
import qualified Network.Wai as Wai

success :: Wai.Response
success = Wai.responseLBS HttpTypes.status200 [] ""

badRequest :: Wai.Response
badRequest = Wai.responseLBS HttpTypes.status400 [] ""

notFound :: Wai.Response
notFound = Wai.responseLBS HttpTypes.status404 [] ""

methodUnsupported :: Wai.Response
methodUnsupported = Wai.responseLBS HttpTypes.status405 [] ""

conflict :: Wai.Response
conflict = Wai.responseLBS HttpTypes.status409 [] ""

redirectTo :: BSC8.ByteString -> Wai.Response
redirectTo dest =
  Wai.responseLBS
    HttpTypes.status302
    [(HttpTypes.hLocation, dest)]
    ""
