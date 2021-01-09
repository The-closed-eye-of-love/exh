module Web.Exhentai.API.Watched
  ( fetchWatched,
    fetchPopular,
  )
where

import Web.Exhentai.Parsing.Search
import Web.Exhentai.Types
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils

-- | Fetch the list of watched galleries
fetchWatched :: MonadHttpState m => m [Gallery]
fetchWatched = do
  d <- htmlRequest' "https://exhentai.org/watched"
  pure $ d ^..: galleryPreviewElement . galleryLink

-- | Fetch the list of popular galleries
fetchPopular :: MonadHttpState m => m [Gallery]
fetchPopular = do
  d <- htmlRequest' "https://exhentai.org/popular"
  pure $ d ^..: galleryPreviewElement . galleryLink
