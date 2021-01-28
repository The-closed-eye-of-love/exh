module Web.Exhentai.API.Watched
  ( fetchWatched,
    fetchPopular,
  )
where

import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Data.Maybe
import Network.HTTP.Client hiding (Cookie)
import Optics.Core
import Web.Exhentai.API.Gallery
import Web.Exhentai.Parsing.Search
import Web.Exhentai.Utils

-- | Fetch the list of watched galleries
fetchWatched :: Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket] m => m [Gallery]
fetchWatched = do
  d <- htmlRequest' "https://exhentai.org/watched"
  pure $ mapMaybe parseGalleryLink $ d ^..: galleryPreviewElement % galleryLink
{-# INLINEABLE fetchWatched #-}

-- | Fetch the list of popular galleries
fetchPopular :: Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket] m => m [Gallery]
fetchPopular = do
  d <- htmlRequest' "https://exhentai.org/popular"
  pure $ mapMaybe parseGalleryLink $ d ^..: galleryPreviewElement % galleryLink
{-# INLINEABLE fetchPopular #-}
