{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Exhentai.API.MPV
  ( DispatchRequest (..),
    DispatchResult (..),
    Vars (..),
    Server (..),
    Dim (..),
    fetchMpv,
    toRequests,
    imageDispatch,
    fetchImage,
  )
where

import Conduit
import Control.Applicative
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import GHC.Generics
import Network.HTTP.Client.Conduit
import Text.XML
import Web.Exhentai.Errors
import Web.Exhentai.Parsing.MPV
import Web.Exhentai.Types
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils

data Server
  = HAtH Int
  | Other Text
  deriving (Show, Eq, Generic)

instance FromJSON Server where
  parseJSON v =
    HAtH <$> parseJSON v <|> Other <$> parseJSON v

instance ToJSON Server where
  toJSON (HAtH i) = toJSON i
  toJSON (Other t) = toJSON t

newtype Dim = Dim Int
  deriving newtype (Show, Eq)

instance FromJSON Dim where
  parseJSON v = Dim <$> str <|> Dim <$> int
    where
      str = read <$> parseJSON v
      int = parseJSON v

data DispatchResult = DispatchResult
  { -- | A piece of text describing the dimensions and the size of this image
    dimension :: Text,
    -- | The path part of the url pointing to the original image
    origImgPath :: Text,
    -- | The path part of the url that searches for the gallery containing this image
    searchPath :: Text,
    -- | The path part of the non-mpv page that displays this image
    galleryPath :: Text,
    width :: Dim,
    height :: Dim,
    -- | The full url to this image
    imgLink :: Text,
    -- | The server that serves this image
    server :: Server
  }
  deriving (Show, Eq, Generic)

instance FromJSON DispatchResult where
  parseJSON = withObject "imagedispatch result" $ \o ->
    DispatchResult
      <$> o .: "d"
      <*> o .: "lf"
      <*> o .: "ls"
      <*> o .: "lo"
      <*> o .: "xres"
      <*> o .: "yres"
      <*> o .: "i"
      <*> o .: "s"

data DispatchRequest = DispatchRequest
  { galleryId :: Int,
    page :: Int,
    imgKey :: Text,
    mpvKey :: Text,
    exclude :: Maybe Server
  }
  deriving (Show, Eq, Generic)

instance ToJSON DispatchRequest where
  toJSON DispatchRequest {..} = object l
    where
      exc = maybe [] (\s -> ["nl" .= s]) exclude
      l =
        exc
          ++ [ "method" .= ("imagedispatch" :: Text),
               "gid" .= galleryId,
               "page" .= page,
               "imgkey" .= imgKey,
               "mpvkey" .= mpvKey
             ]

toRequests :: Vars -> [DispatchRequest]
toRequests Vars {..} = zipWith formReq [1 ..] imageList
  where
    formReq i MpvImage {..} =
      DispatchRequest
        { galleryId = gid,
          page = i,
          imgKey = key,
          exclude = Nothing,
          mpvKey = mpvkey
        }

fetchMpv :: (MonadHttpState m, MonadIO m) => Gallery -> m Vars
fetchMpv g = htmlRequest' (toMpvLink g) >>= parseMpv

parseMpv :: (MonadIO m, MonadThrow m) => Document -> m Vars
parseMpv doc = do
  let script = doc ^. allScripts
  res <- liftIO $ extractEnv script
  case res of
    Error e -> throwM $ ExtractionFailure e
    Success vars -> pure vars

imageDispatch :: MonadHttpState m => DispatchRequest -> m DispatchResult
imageDispatch dreq = do
  initReq <- formRequest "https://exhentai.org/api.php"
  let req = initReq {method = "POST", requestBody = RequestBodyLBS $ encode dreq}
  r <- jsonRequest req
  case r of
    Left e -> throwM $ JSONParseFailure e
    Right res -> pure res

fetchImage :: (MonadHttpState m, MonadIO n) => DispatchRequest -> m (Response (ConduitT i ByteString n ()))
fetchImage dreq = do
  res <- imageDispatch dreq
  req <- formRequest $ unpack $ imgLink res
  openWithJar req `catch` \(_ :: HttpException) -> do
    res' <- imageDispatch $ dreq {exclude = Just $ server res}
    req' <- formRequest $ unpack $ imgLink res'
    openWithJar req' `catch` \(_ :: HttpException) -> do
      req'' <- formRequest $ unpack $ "https://exhentai.org/" <> origImgPath res
      openWithJar req''
