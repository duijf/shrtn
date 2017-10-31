module State where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import URI.ByteString (URIRef, Relative, Absolute, URIParseError)
import qualified Data.Char as Char
import qualified URI.ByteString as URI
import qualified System.Random as Random

newtype Slug
  = Slug (URIRef Relative)

newtype Dest
  = Dest (URIRef Absolute)

newSlug :: ByteString -> Either URIParseError Slug
newSlug path =
  fmap Slug $
    URI.parseRelativeRef
      URI.strictURIParserOptions
      path

createSlug :: IO Slug
createSlug = do
  rng <- Random.getStdGen
  pure $ createSlug' rng

createSlug' :: Random.StdGen -> Slug
createSlug' rng =
  let
    rndChars = Random.randomRs ('A', 'z') rng
    rndAlphaNum = filter Char.isAlphaNum rndChars
  in
    Slug . (\s -> URI.RelativeRef {URI.rrPath = s}) . ByteString.pack . (take 8) $ rndAlphaNum

newDest :: ByteString -> Either URIParseError Dest
newDest uri
  = fmap Dest $ URI.parseURI URI.strictURIParserOptions uri


