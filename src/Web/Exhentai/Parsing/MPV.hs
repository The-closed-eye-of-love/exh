{-# LANGUAGE StrictData #-}

module Web.Exhentai.Parsing.MPV where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Quickjs
import Text.XML.Lens
import Web.Exhentai.Utils

allScripts :: Traversal' Document Text
allScripts = body ... scripts . lower . _Content

data MpvImage = MpvImage
  { name :: Text,
    key :: Text,
    thumbnail :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MpvImage where
  parseJSON = withObject "mpv image" $ \o ->
    MpvImage
      <$> o .: "n"
      <*> o .: "k"
      <*> o .: "t"

data Vars = Vars
  { gid :: Int,
    mpvkey :: Text,
    apiUrl :: Text,
    pageCount :: Int,
    imageList :: [MpvImage]
  }
  deriving (Show, Eq, Generic)

extractEnv :: Text -> IO (Result Vars)
extractEnv script = quickjs $ do
  eval_ $ encodeUtf8 script
  gid' <- eval "gid"
  mpvkey' <- eval "mpvkey"
  imageList' <- eval "imagelist"
  apiUrl' <- eval "api_url"
  pageCount' <- eval "pagecount"
  pure $ do
    gid <- fromJSON gid'
    mpvkey <- fromJSON mpvkey'
    imageList <- fromJSON imageList'
    apiUrl <- fromJSON apiUrl'
    pageCount <- fromJSON pageCount'
    pure Vars {..}
