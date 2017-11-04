{-# LANGUAGE TemplateHaskell #-}

module State where

import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.Acid as Acid
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Typeable as Typeable
import qualified Data.Text as Text
import           URI.ByteString (URIRef, Relative, Absolute, URIParseError)
import qualified URI.ByteString as URI
import qualified System.Random as Random

newtype Alias = Alias ByteString deriving (Eq, Ord)
newtype Dest = Dest (URIRef Absolute)

data AliasMap = AliasMap (Map Alias Dest)
data ShrtnState = ShrtnState
  { _aliasMap :: !AliasMap
  }

Lens.makePrisms ''AliasMap
Lens.makeLenses ''ShrtnState

-- ----------------------------------------------------------------------------
-- Parsing and creating

data AliasParseErr
  = CharLimitExceeded
  | IllegalCharacters


-- TODO: Refactor to use a proper parser, this will turn hairy.
parseAlias :: ByteString -> Either AliasParseErr Alias
parseAlias path =
  if ByteString.length path > 32
  then Left CharLimitExceeded
  else
    if not $ ByteString.all Char.isAlphaNum path
    then Left IllegalCharacters
    else Right (Alias path)


parseDest :: ByteString -> Either URIParseError Dest
parseDest uri
  = fmap Dest $ URI.parseURI URI.strictURIParserOptions uri


createAlias :: IO Alias
createAlias = do
  rng <- Random.getStdGen
  pure $ createAlias' rng


createAlias' :: Random.StdGen -> Alias
createAlias' rng =
  let
    rndChars = Random.randomRs ('A', 'z') rng
    rndAlphaNum = filter Char.isAlphaNum rndChars
  in
    Alias . ByteString.pack . (take 8) $ rndAlphaNum

-- ----------------------------------------------------------------------------
-- State operations

-- TODO: Fail if the alias is already in the state map
insertAlias :: Alias -> Dest -> Acid.Update ShrtnState ()
insertAlias alias dest = do
  state <- State.get

  let
    path = aliasMap . _AliasMap . (Lens.at alias) . Lens._Just
    newState = Lens.set path dest state

  State.put newState


lookupDest :: Alias -> Acid.Query ShrtnState (Maybe Dest)
lookupDest alias = do
  state <- Reader.ask
  return $ Lens.view (aliasMap . _AliasMap . Lens.at alias) state
