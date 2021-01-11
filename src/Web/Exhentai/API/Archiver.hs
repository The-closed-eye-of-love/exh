module Web.Exhentai.API.Archiver
  ( streamOriginal,
    streamResampled,
  )
where

import Conduit
import Control.Lens (Traversal')
import Control.Monad.Catch
import Control.Monad.Cont
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.MultipartFormData
import Text.XML.Lens
import Web.Exhentai.Errors
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils
import Prelude hiding (id)

downloadLink :: Traversal' Element Text
downloadLink = id "db" ... id "continue" ... attr "href"

originalParts :: [Part]
originalParts =
  [ partBS "dltype" "org",
    partBS "dlcheck" "Download Original Archive"
  ]
{-# INLINE originalParts #-}

resampledParts :: [Part]
resampledParts =
  [ partBS "dltype" "res",
    partBS "dlcheck" "Download Resample Archive"
  ]
{-# INLINE resampledParts #-}

streamWith :: (MonadHttpState m, MonadIO n) => [Part] -> Text -> ContT r m (Response (ConduitT i ByteString n ()))
streamWith parts url = ContT $ \k -> do
  initReq <- formRequest $ unpack url
  req <- formDataBody parts initReq
  d <- htmlRequest req
  case d ^?: downloadLink of
    Nothing -> throwM $ XMLParseFailure "download link" url
    Just l -> do
      newReq <- formRequest $ unpack l
      let req' = setQueryString [("start", Just "1")] newReq
      retryWhenTimeout $
        bracket
          (respOpen req')
          respClose
          k

-- | Download an origian archive from an archiver url as a stream
streamOriginal ::
  (MonadHttpState m, MonadIO n) =>
  -- | Archiver url, usually the 'archiverLink` field
  Text ->
  ContT r m (Response (ConduitT i ByteString n ()))
streamOriginal = streamWith originalParts

-- | Download an resampled archive from an archiver url as a stream
streamResampled ::
  (MonadHttpState m, MonadIO n) =>
  -- | Archiver url, usually the 'archiverLink` field
  Text ->
  ContT r m (Response (ConduitT i ByteString n ()))
streamResampled = streamWith resampledParts
