{-# LANGUAGE StrictData #-}

module Web.Exhentai.API.Search
  ( SearchQuery (..),
    SearchResult (..),
    search,
    searchRecur,
    searchRecurResumable,
    searchRecurResumable',
    fetchSearchPage,
    fetchSearchPage',
  )
where

import Conduit
import Control.Lens ((...))
import Control.Lens.Fold
import Control.Monad
import Data.Set (Set, (\\))
import Data.String
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Client.Conduit
import Text.XML
import Web.Exhentai.Parsing.Search
import Web.Exhentai.Types
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils
import Prelude hiding (last)

data SearchQuery = SearchQuery
  { categories :: Maybe (Set GalleryCat),
    searchString :: {-# UNPACK #-} Text
  }
  deriving (Show, Eq, Generic)

queryArgCat :: Set GalleryCat -> Int
queryArgCat s = toBitField $ allGalleryCats \\ s

data SearchResult = SearchResult
  { galleries :: [Gallery],
    prevPage :: Maybe Text,
    nextPage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

parseSearchPage :: Document -> SearchResult
parseSearchPage d =
  let galleries = d ^..: galleryPreviewElement . galleryLink
      fld :: Fold Document Element
      fld = body ... pagesElem
      prevPage = do
        first <- firstOf fld d
        first ^? linkOf
      nextPage = do
        last <- lastOf fld d
        last ^? linkOf
   in SearchResult {..}

-- | Fetch a search page using a 'Request'
fetchSearchPage' :: MonadHttpState m => Request -> m SearchResult
fetchSearchPage' req = do
  d <- htmlRequest req
  pure $ parseSearchPage d

-- | Fetch a search page using its url
fetchSearchPage :: MonadHttpState m => Text -> m SearchResult
fetchSearchPage = fetchSearchPage' <=< formRequest . unpack

-- | Search a search query
search :: MonadHttpState m => SearchQuery -> m SearchResult
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

-- | Iterate through all the Galleries asosciated with a search query, putting them into a stream
searchRecur :: MonadHttpState m => SearchQuery -> ConduitT i Gallery m ()
searchRecur q = do
  SearchResult {..} <- lift $ search q
  yieldMany galleries
  case nextPage of
    Nothing -> pure ()
    Just url -> searchRecur' url

searchRecur' ::
  MonadHttpState m =>
  -- | url
  Text ->
  ConduitT i Gallery m ()
searchRecur' url = do
  SearchResult {..} <- lift $ fetchSearchPage url
  yieldMany galleries
  case nextPage of
    Nothing -> pure ()
    Just url' -> searchRecur' url'

-- | A resumable version of 'searchRecur' that reports it's progress.
searchRecurResumable :: MonadHttpState m => SearchQuery -> ConduitT i (Either Text Gallery) m ()
searchRecurResumable q = do
  SearchResult {..} <- lift $ search q
  yieldMany $ map Right galleries
  case nextPage of
    Nothing -> pure ()
    Just url -> searchRecurResumable' url

searchRecurResumable' ::
  MonadHttpState m =>
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
