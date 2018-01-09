{-# LANGUAGE OverloadedStrings #-}

module Shrtn.Views where

import qualified Data.ByteString.Lazy.Char8 as BSLC8
import           Lucid

mngmtView :: BSLC8.ByteString
mngmtView = renderBS $
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ "shrtn"
      link_ [rel_ "stylesheet", href_ "style.css"]
    body_ $ do
      h1_ "Shrtn an URL"
      form_ [action_ "/aliases", method_ "POST", id_ "form"] $ do
        input_ [type_ "text", name_ "dest", id_ "dest", placeholder_ "URL to shrtn", required_ "required"]
        br_ []
        button_ [type_ "submit", id_ "submit"] "Shrtn URL"
        br_ []
        small_ [id_ "toggle-alias"] "Want a custom alias?"
      script_ "TODO"
