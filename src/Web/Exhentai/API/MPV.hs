{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MPV (multi-page viewer) API.
module Web.Exhentai.API.MPV
  ( DispatchRequest (..),
    Server (..),
    Dim (..),
    buildRequest,
    fetchImage,
  )
where

import Conduit
import Control.Applicative
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Control.Monad
import Control.Monad.Trans.Cont
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Language.JavaScript.Extraction
import Language.JavaScript.Parser
import Network.HTTP.Client hiding (Cookie)
import Optics.Core
import Optics.TH
import Text.XML
import Text.XML.Optics
import Web.Exhentai.Errors
import Web.Exhentai.Types
import Web.Exhentai.Utils
import Prelude hiding ((!!))

data Server
  = HAtH {-# UNPACK #-} Int
  | Other {-# UNPACK #-} Text
  deriving (Show, Eq)

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
    dimension :: {-# UNPACK #-} Text,
    -- | The path part of the url pointing to the original image
    origImgPath :: {-# UNPACK #-} Text,
    -- | The path part of the url that searches for the gallery containing this image
    searchPath :: {-# UNPACK #-} Text,
    -- | The path part of the non-mpv page that displays this image
    galleryPath :: {-# UNPACK #-} Text,
    width :: {-# UNPACK #-} Dim,
    height :: {-# UNPACK #-} Dim,
    -- | The full url to this image
    imgLink :: {-# UNPACK #-} Text,
    -- | The server that serves this image
    server :: Server
  }
  deriving (Show, Eq)

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
  { galleryId :: {-# UNPACK #-} Int,
    page :: {-# UNPACK #-} Int,
    imgKey :: {-# UNPACK #-} Text,
    mpvKey :: {-# UNPACK #-} Text,
    exclude :: Maybe Server
  }
  deriving (Show, Eq)

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

-- | Generate a list of requests from a 'Vars'
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

-- | Fetch the 'Vars' from a Gallery's mpv page
fetchMpv ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  Gallery ->
  m Vars
fetchMpv g = htmlRequest' (toMpvLink g) >>= parseMpv
{-# INLINEABLE fetchMpv #-}

parseMpv :: Effs '[Throw ExhentaiError] m => Document -> m Vars
parseMpv doc = do
  let script = foldOf allScripts doc
      mast = parse (unpack script) ""
  case mast of
    Left _ -> error "impossible, javascript parse failed"
    Right ast ->
      case as ast of
        Nothing -> throw ExtractionFailure
        Just vars -> pure vars
{-# INLINEABLE parseMpv #-}

-- | Build dispatch requests for a gallery
buildRequest ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  Gallery ->
  m [DispatchRequest]
buildRequest g = toRequests <$> fetchMpv g
{-# INLINEABLE buildRequest #-}

-- | Calls the API to dispatch a image request to a H@H server
imageDispatch ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  DispatchRequest ->
  m DispatchResult
imageDispatch dreq = do
  initReq <- formRequest "https://exhentai.org/api.php"
  let req = initReq {method = "POST", requestBody = RequestBodyLBS $ encode dreq}
  r <- jsonRequest req
  case r of
    Left e -> throw $ JSONParseFailure e
    Right res -> pure res
{-# INLINEABLE imageDispatch #-}

-- | Fetch an image with a 'DispatchRequest'
fetchImage ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  DispatchRequest ->
  ContT r m (Response (ConduitT i ByteString IO ()))
fetchImage dreq = ContT $ \k -> bracket (fetchImage' dreq) respClose k
{-# INLINEABLE fetchImage #-}

-- | Like 'fetchImage', but the user is responsible of closing the response
fetchImage' ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  DispatchRequest ->
  m (Response (ConduitT i ByteString IO ()))
fetchImage' dreq = do
  res <- imageDispatch dreq
  req <- formRequest $ unpack $ imgLink res
  openWithJar req `catch` \(_ :: HttpException) -> do
    res' <- imageDispatch $ dreq {exclude = Just $ server res}
    req' <- formRequest $ unpack $ imgLink res'
    openWithJar req' `catch` \(_ :: HttpException) -> do
      req'' <- formRequest $ unpack $ "https://exhentai.org/" <> origImgPath res
      openWithJar req''
{-# INLINEABLE fetchImage' #-}

allScripts :: Traversal' Document Text
allScripts = body .// (scripts % lower %> _Content)

makeFieldLabelsWith noPrefixFieldLabels ''DispatchResult
makeFieldLabelsWith noPrefixFieldLabels ''DispatchRequest
makePrismLabels ''Dim
makePrismLabels ''Server
