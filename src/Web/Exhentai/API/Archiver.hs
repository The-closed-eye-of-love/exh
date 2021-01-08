module Web.Exhentai.API.Archiver where

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
    Nothing -> throwM $ XMLParseFailure url
    Just l -> do
      newReq <- formRequest $ unpack l
      let req' = setQueryString [("start", Just "1")] newReq
      retryWhenTimeout $
        bracket
          (respOpen req')
          respClose
          k

streamOriginal :: (MonadHttpState m, MonadIO n) => Text -> ContT r m (Response (ConduitT i ByteString n ()))
streamOriginal = streamWith originalParts

streamResampled :: (MonadHttpState m, MonadIO n) => Text -> ContT r m (Response (ConduitT i ByteString n ()))
streamResampled = streamWith resampledParts
