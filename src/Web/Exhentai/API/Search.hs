{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Exhentai.API.Search
  ( SearchQuery (..),
    SearchResult (..),
    search,
    searchRecur,
    searchRecurResumable,
    fetchSearchPage,
  )
where

import Conduit
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Control.Monad
import Data.Maybe
import Data.Set (Set, toList, (\\))
import Data.String
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client hiding (Cookie)
import Optics.Core
import Optics.TH
import Text.XML.Optics
import Web.Exhentai.API.Gallery
import Web.Exhentai.Parsing.Search
import Web.Exhentai.Utils
import Prelude hiding (last)

toBitField :: Set GalleryCategory -> Int
toBitField = sum . map ((2 ^) . fromEnum) . toList

data SearchQuery = SearchQuery
  { categories :: Maybe (Set GalleryCategory),
    searchString :: {-# UNPACK #-} Text
  }
  deriving (Show, Eq)

queryArgCat :: Set GalleryCategory -> Int
queryArgCat s = toBitField $ allGalleryCats \\ s

data SearchResult = SearchResult
  { galleries :: [Gallery],
    prevPage :: Maybe Text,
    nextPage :: Maybe Text
  }
  deriving (Show, Eq)

parseSearchPage :: Document -> SearchResult
parseSearchPage d =
  let galleries = mapMaybe parseGalleryLink $ d ^..: galleryPreviewElement % galleryLink
      fld :: Traversal' Document Element
      fld = body .// pagesElem
      prevPage = do
        first <- headOf fld d
        first ^? pre linkOf
      nextPage = do
        last <- lastOf fld d
        last ^? pre linkOf
   in SearchResult {..}

-- | Fetch a search page using a 'Request'
fetchSearchPage' ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  Request ->
  m SearchResult
fetchSearchPage' req = do
  d <- htmlRequest req
  pure $ parseSearchPage d
{-# INLINEABLE fetchSearchPage' #-}

-- | Fetch a search page using its url
fetchSearchPage ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  Text ->
  m SearchResult
fetchSearchPage = fetchSearchPage' <=< formRequest . unpack
{-# INLINEABLE fetchSearchPage #-}

-- | Search a search query
search ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  SearchQuery ->
  m SearchResult
search SearchQuery {..} = do
  let catQ = maybe [] (\c -> [("f_cats", Just $ fromString $ show $ queryArgCat c)]) categories
  initReq <- formRequest "https://exhentai.org"
  let req =
        setQueryString
          ( catQ
              ++ [ ("f_search", Just $ encodeUtf8 searchString)
                 ]
          )
          initReq
  fetchSearchPage' req
{-# INLINEABLE search #-}

-- | Iterate through all the Galleries asosciated with a search query, putting them into a stream
searchRecur ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  SearchQuery ->
  ConduitT i Gallery m ()
searchRecur q = do
  SearchResult {..} <- lift $ search q
  yieldMany galleries
  case nextPage of
    Nothing -> pure ()
    Just url -> searchRecur' url
{-# INLINEABLE searchRecur #-}

searchRecur' ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  -- | url
  Text ->
  ConduitT i Gallery m ()
searchRecur' url = do
  SearchResult {..} <- lift $ fetchSearchPage url
  yieldMany galleries
  case nextPage of
    Nothing -> pure ()
    Just url' -> searchRecur' url'
{-# INLINEABLE searchRecur' #-}

-- | A resumable version of 'searchRecur' that reports it's progress.
searchRecurResumable ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  SearchQuery ->
  ConduitT i (Either Text Gallery) m ()
searchRecurResumable q = do
  SearchResult {..} <- lift $ search q
  yieldMany $ map Right galleries
  case nextPage of
    Nothing -> pure ()
    Just url -> searchRecurResumable' url
{-# INLINEABLE searchRecurResumable #-}

searchRecurResumable' ::
  Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m =>
  -- | url
  Text ->
  ConduitT i (Either Text Gallery) m ()
searchRecurResumable' url = do
  yield $ Left url
  SearchResult {..} <- lift $ fetchSearchPage url
  yieldMany $ map Right galleries
  case nextPage of
    Nothing -> pure ()
    Just url' -> searchRecurResumable' url'
{-# INLINEABLE searchRecurResumable' #-}

makeFieldLabelsWith noPrefixFieldLabels ''SearchQuery
makeFieldLabelsWith noPrefixFieldLabels ''SearchResult
