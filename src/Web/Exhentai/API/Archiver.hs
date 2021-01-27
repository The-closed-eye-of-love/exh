module Web.Exhentai.API.Archiver
  ( streamOriginal,
    streamResampled,
  )
where

import Conduit
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Control.Monad.Trans.Cont
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Network.HTTP.Client hiding (Cookie)
import Network.HTTP.Client.MultipartFormData
import Optics.Core (Traversal')
import Text.XML.Optics
import Web.Exhentai.Errors
import Web.Exhentai.Utils
import Prelude hiding (id)

downloadLink :: Traversal' Element Text
downloadLink = id "db" .// id "continue" .// attr "href"

originalParts :: Applicative m => [PartM m]
originalParts =
  [ partBS "dltype" "org",
    partBS "dlcheck" "Download Original Archive"
  ]
{-# INLINE originalParts #-}

resampledParts :: Applicative m => [PartM m]
resampledParts =
  [ partBS "dltype" "res",
    partBS "dlcheck" "Download Resample Archive"
  ]
{-# INLINE resampledParts #-}

streamWith ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  [PartM m] ->
  Text ->
  ContT r m (Response (ConduitT i ByteString IO ()))
streamWith parts url = ContT $ \k -> do
  initReq <- formRequest $ unpack url
  req <- attachFormData parts initReq
  d <- htmlRequest req
  case d ^?: downloadLink of
    Nothing -> throw $ XMLParseFailure "download link" url
    Just l -> do
      newReq <- formRequest $ unpack l
      let req' = setQueryString [("start", Just "1")] newReq
      bracket
        (respOpen req')
        respClose
        (k . fmap bodyReaderSource)

-- | Download an origian archive from an archiver url as a stream
streamOriginal ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  -- | Archiver url, usually the 'archiverLink` field
  Text ->
  ContT r m (Response (ConduitT i ByteString IO ()))
streamOriginal = streamWith originalParts

-- | Download an resampled archive from an archiver url as a stream
streamResampled ::
  Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket, Throw ExhentaiError] m =>
  -- | Archiver url, usually the 'archiverLink` field
  Text ->
  ContT r m (Response (ConduitT i ByteString IO ()))
streamResampled = streamWith resampledParts
