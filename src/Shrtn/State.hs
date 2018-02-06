{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Shrtn.State
  ( new
  , Handle
  , run
  , insertIfNotExists
  , InsertResult(..)
  , insertRandom
  , lookupDest
  , listAll
  , STM.atomically
  , State
  , Slug
  , Dest
  ) where

import           Control.Concurrent.STM (STM, TVar, TChan)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL8
import           Data.Default (Default, def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.Random as Random

type Slug = Text
type Dest = Text
type State = HashMap Slug Dest

data Config = Config
  { cStateFile :: String
  }

instance Default Config where
  def = Config { cStateFile = "shrtn.state" }

data Handle = Handle
  { hConfig :: Config
  , hState :: TVar State
  , hRng :: TVar Random.StdGen
  , hStateChan :: TChan State
  }

new :: Config -> IO Handle
new config = do
  state <- openState $ cStateFile config
  stateChan <- STM.atomically STM.newTChan
  rng <- (Random.getStdGen >>= \r -> STM.atomically $ STM.newTVar r)
  return $ Handle
    { hConfig = config
    , hState = state
    , hRng = rng
    , hStateChan = stateChan
    }

run :: Handle -> IO ()
run h@Handle{..} = do
  state <- STM.atomically $ STM.readTChan hStateChan
  BSL8.writeFile (cStateFile hConfig) (Aeson.encode state)
  run h

openState :: String -> IO (TVar State)
openState statePath = do
  exists <- Directory.doesFileExist statePath
  if exists
    then openExisting statePath
    else openDefault statePath

openExisting :: String -> IO (TVar State)
openExisting statePath = do
  putStrLn $ ":: Found existing state file in " ++ statePath
  contents <- BSL8.readFile statePath
  case Aeson.decode contents of
    Just state -> STM.atomically $ STM.newTVar state
    Nothing -> do
      putStrLn $ ":: Existing state corrupt"
      openDefault statePath

openDefault :: String -> IO (TVar State)
openDefault statePath = do
  putStrLn $ ":: Opening new state file in " ++ statePath
  STM.atomically $ STM.newTVar $ HashMap.empty

data InsertResult = SlugAlreadyExists | InsertSuccess

insertRandom :: Handle -> Dest -> STM InsertResult
insertRandom h@Handle{..} dest = do
  actualRng <- STM.readTVar hRng
  let (slug, rng') = randomSlug actualRng
  insertResult <- insertIfNotExists h slug dest
  STM.writeTVar hRng rng'
  case insertResult of
    InsertSuccess -> pure InsertSuccess
    SlugAlreadyExists -> insertRandom h dest

-- TODO: Crappy roll-your-own shit, refactor into state monad?
randomSlug :: Random.StdGen -> (Slug, Random.StdGen)
randomSlug rng = undefined
  where
    f :: Random.StdGen -> Int -> ([Char], Random.StdGen)
    f r 0 = ([], r)
    f r n =
      let
        (a, r') = Random.randomR ('a', 'z') rng
        (as, r'') = f r' (n-1)
      in
        (a:as, r'')

insertIfNotExists :: Handle -> Slug -> Dest -> STM InsertResult
insertIfNotExists Handle{..} slug dest = do
  redirectMap <- STM.readTVar hState
  if HashMap.member slug redirectMap
    then pure SlugAlreadyExists
    else do
      let newState = HashMap.insert slug dest redirectMap
      STM.writeTVar hState newState
      STM.writeTChan hStateChan newState
      pure InsertSuccess

lookupDest :: Handle -> Slug -> STM (Maybe Dest)
lookupDest Handle{..} slug = do
  redirectMap <- STM.readTVar hState
  pure $ HashMap.lookup slug redirectMap

listAll :: Handle -> STM (HashMap Slug Dest)
listAll Handle{..} = STM.readTVar hState
