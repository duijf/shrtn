{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  Warp.runSettings
    shrtnSettings
    shrtn

shrtnSettings :: Warp.Settings
shrtnSettings
  = Warp.setPort 7000
  $ Warp.setHost "*"
  $ Warp.defaultSettings

shrtn :: Wai.Application
shrtn _request respond =
  respond $
    Wai.responseLBS
      HTTP.status200
      []
      "Things are working pretty nicely."
