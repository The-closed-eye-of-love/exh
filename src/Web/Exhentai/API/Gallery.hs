{-# LANGUAGE StrictData #-}

module Web.Exhentai.API.Gallery
  ( GalleryInfo (..),
    Visibility (..),
    fetchGalleryInfo,
    parseGallery,
  )
where

import Control.Lens ((^..), (^?))
import Control.Monad
import Control.Monad.Catch
import Data.Coerce
import Data.Text (Text, strip)
import Data.Time
import GHC.Generics
import Text.XML
import Text.XML.Lens
import Web.Exhentai.Errors
import qualified Web.Exhentai.Parsing.Gallery as G
import Web.Exhentai.Types
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils
import Prelude hiding (length)
import qualified Prelude as P

-- | Information about a gallery
data GalleryInfo = GalleryInfo
  { title :: {-# UNPACK #-} Text,
    previewLink :: {-# UNPACK #-} Text,
    category :: GalleryCat,
    jaTitle :: Maybe Text,
    uploader :: {-# UNPACK #-} Text,
    rating :: {-# UNPACK #-} Float,
    ratingCount :: {-# UNPACK #-} Int,
    favoriteCount :: {-# UNPACK #-} Int,
    tags :: [(G.TagCategory, [Text])],
    uploadTime :: {-# UNPACK #-} UTCTime,
    newer :: Maybe Gallery,
    parent :: Maybe Gallery,
    visibility :: Visibility,
    language :: {-# UNPACK #-} Text,
    length :: {-# UNPACK #-} Int,
    archiverLink :: {-# UNPACK #-} Text
  }
  deriving (Show, Eq, Generic)

data Visibility
  = Visible
  | Replaced
  | Expunged
  | Unknown {-# UNPACK #-} Text
  deriving (Show, Eq, Generic)

readVisibility :: Text -> Visibility
readVisibility "Yes" = Visible
readVisibility "No (Replaced)" = Replaced
readVisibility "No (Expunged)" = Expunged
readVisibility v = Unknown v

-- | Extract all gallery informations from a document
parseGallery :: Document -> Either Text GalleryInfo
parseGallery d = do
  title <- annotate "title" $ d ^?: G.enTitle
  previewLink <- annotate "preview link" $ d ^?: G.previewStr >>= parsePreviewLink
  let jaTitle = d ^?: G.jaTitle
  category <- annotate "category" $ d ^?: G.category
  uploader <- annotate "uploader" $ d ^?: G.uploader
  (coerce -> rating) <- annotate "average rating" $ d ^?: G.averageRating
  ratingCount <- annotate "rating count" $ d ^?: G.ratingCount
  (coerce -> archiverLink) <- annotate "archiver link" $ d ^?: G.archiverLink
  let newer = d ^?: G.newer
  case d ^..: G.metaValues of
    (time : parn : vis : lang : _ : len : fav : _) -> do
      uploadTime <- annotate "upload time" $ time ^? lower . _Content >>= parseUploadTime
      let parent = parn ^? lower . _Element . attr "href" . _GalleryLink
      (readVisibility -> visibility) <- annotate "visibility" $ vis ^? lower . _Content
      (strip -> language) <- annotate "language" $ lang ^? lower . _Content
      (coerce -> length) <- annotate "length" $ len ^? lower . _Content >>= parseGalleryLength
      let cats = d ^..: G.tagCategory
      let tags' = map (^.. G.tags) $ d ^..: G.tagsByCategory
      unless (P.length cats == P.length tags') $ Left ""
      let tags = zip cats tags'
      (coerce -> favoriteCount) <- annotate "favorite count" $ fav ^? lower . _Content >>= parseFavoriteCount
      pure GalleryInfo {..}
    _ -> Left "extracting metadata"

-- | Fetch a gallery's 'GalleryInfo'
fetchGalleryInfo :: MonadHttpState m => Gallery -> m GalleryInfo
fetchGalleryInfo g = do
  let url = toGalleryLink g
  d <- htmlRequest' url
  case parseGallery d of
    Left err -> throwM $ XMLParseFailure err url
    Right info -> pure info
