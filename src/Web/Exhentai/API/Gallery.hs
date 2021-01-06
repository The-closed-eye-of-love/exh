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

data GalleryInfo = GalleryInfo
  { title :: {-# UNPACK #-} Text,
    category :: GalleryCat,
    jaTitle :: {-# UNPACK #-} Text,
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
  | Other {-# UNPACK #-} Text
  deriving (Show, Eq, Generic)

readVisibility :: Text -> Visibility
readVisibility "Yes" = Visible
readVisibility "No (Replaced)" = Replaced
readVisibility "No (Expunged)" = Expunged
readVisibility v = Other v

parseGallery :: Document -> Maybe GalleryInfo
parseGallery d = do
  title <- d ^?: G.enTitle
  jaTitle <- d ^?: G.jaTitle
  category <- d ^?: G.category
  uploader <- d ^?: G.uploader
  (coerce -> rating) <- d ^?: G.averageRating
  ratingCount <- d ^?: G.ratingCount
  (coerce -> archiverLink) <- d ^?: G.archiverLink
  let newer = d ^?: G.newer
  case d ^..: G.metaValues of
    (time : parn : vis : lang : _ : len : fav : _) -> do
      uploadTime <- time ^? lower . _Content >>= parseUploadTime
      let parent = parn ^? lower . _Element . attr "href" . _GalleryLink
      (readVisibility -> visibility) <- vis ^? lower . _Content
      (strip -> language) <- lang ^? lower . _Content
      (coerce -> length) <- len ^? lower . _Content >>= parseGalleryLength
      let cats = d ^..: G.tagCategory
      let tags' = map (^.. G.tags) $ d ^..: G.tagsByCategory
      guard $ P.length cats == P.length tags'
      let tags = zip cats tags'
      (coerce -> favoriteCount) <- fav ^? lower . _Content >>= parseFavoriteCount
      pure GalleryInfo {..}
    _ -> Nothing

fetchGalleryInfo :: MonadHttpState m => Gallery -> m GalleryInfo
fetchGalleryInfo g = do
  let url = toGalleryLink g
  d <- htmlRequest' url
  case parseGallery d of
    Nothing -> throwM $ XMLParseFailure url
    Just info -> pure info
