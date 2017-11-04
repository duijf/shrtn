{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module State where

import qualified Control.Lens as Lens
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import           Data.Acid (AcidState(..))
import qualified Data.Acid as Acid
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Char as Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (SafeCopy)
import qualified Data.SafeCopy as SafeCopy
import qualified Data.Typeable as Typeable
import           URI.ByteString (URI, Absolute, URIRef(..), URIParseError)
import qualified URI.ByteString as URI
import qualified System.Random as Random

newtype Alias = Alias ByteString deriving (Eq, Ord)
newtype Dest = Dest URI

data AliasMap = AliasMap (Map Alias Dest)
data ShrtnState = ShrtnState
  { _aliasMap :: !AliasMap
  }

Lens.makePrisms ''AliasMap
Lens.makeLenses ''ShrtnState

$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.Scheme)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.UserInfo)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.Host)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.Port)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.Authority)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''URI.Query)

instance SafeCopy (URIRef Absolute) where
  version = 0
  getCopy = SafeCopy.contain $ URI <$> SafeCopy.safeGet
                                   <*> SafeCopy.safeGet
                                   <*> SafeCopy.safeGet
                                   <*> SafeCopy.safeGet
                                   <*> SafeCopy.safeGet
  putCopy URI{..} = SafeCopy.contain $ do
    SafeCopy.safePut uriScheme
    SafeCopy.safePut uriAuthority
    SafeCopy.safePut uriPath
    SafeCopy.safePut uriQuery
    SafeCopy.safePut uriFragment


$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''Alias)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''Dest)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''AliasMap)
$(SafeCopy.deriveSafeCopy 0 'SafeCopy.base ''ShrtnState)

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


blah :: Alias
blah = Alias "blah"

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


$(Acid.makeAcidic ''ShrtnState ['insertAlias, 'lookupDest])


openState :: String -> IO AppST
openState filePath =
  let
    empty = ShrtnState { _aliasMap = AliasMap (Map.empty) }
  in
    fmap AppST $ Acid.openLocalStateFrom filePath empty

newtype AppST = AppST (AcidState ShrtnState)

insert alias dest (AppST state) = AppST $ Acid.update state (insertAlias alias dest)

lookup :: Alias -> AppST -> Maybe Dest
lookup alias (AppST state) = Acid.query state (lookupDest alias)

testDest :: Dest
testDest =
  let
    parsed = URI.parseURI URI.strictURIParserOptions "http://example.com"
    uri = right parsed
  in
    Dest uri

right :: Either a b -> b
right (Right a) = a
