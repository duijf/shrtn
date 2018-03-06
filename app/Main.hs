{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Concurrent.Async as Async
import           Data.Default (def)

import qualified Shrtn.Http as Http
import qualified Shrtn.State as State

main :: IO ()
main = do
  state <- State.new def
  http <- Http.new state def

  foldr1 Async.race_ [Http.run http, State.run state]
