{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Exhentai.Types where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Set (Set, fromList, toList)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, takeWhile1P),
    Parsec,
    anySingle,
    chunk,
    many,
    optional,
    parseMaybe,
    single,
    takeRest,
  )
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data GalleryCat
  = Misc
  | Doujinshi
  | Manga
  | ArtistCG
  | GameCG
  | ImageSet
  | Cosplay
  | AsianPorn
  | NonH
  | Western
  | Private
  deriving (Show, Eq, Ord, Enum, Bounded)

allGalleryCats :: Set GalleryCat
allGalleryCats = fromList [Misc .. Private]

toBitField :: Set GalleryCat -> Int
toBitField = sum . map ((2 ^) . fromEnum) . toList

showCat :: GalleryCat -> Text
showCat Doujinshi = "Doujinshi"
showCat Manga = "Manga"
showCat ArtistCG = "Artist CG"
showCat GameCG = "Game CG"
showCat NonH = "Non-H"
showCat ImageSet = "Image Set"
showCat Western = "Western"
showCat Cosplay = "Cosplay"
showCat Misc = "Misc"
showCat Private = "Private"
showCat AsianPorn = "Asian Porn"

readCat :: Text -> Maybe GalleryCat
readCat "Doujinshi" = Just Doujinshi
readCat "Manga" = Just Manga
readCat "Artist CG" = Just ArtistCG
readCat "Game CG" = Just GameCG
readCat "Non-H" = Just NonH
readCat "Image Set" = Just ImageSet
readCat "Western" = Just Western
readCat "Cosplay" = Just Cosplay
readCat "Misc" = Just Misc
readCat "Private" = Just Private
readCat "Asian Porn" = Just AsianPorn
readCat _ = Nothing

_GalleryCat :: Prism' Text GalleryCat
_GalleryCat = prism' showCat readCat

newtype PopUpLink = PopUpLink {unLink :: Text}
  deriving newtype (Show, Eq)

_PopUpLink :: Prism' Text PopUpLink
_PopUpLink = prism' unLink parsePopUpLink

parsePopUpLink :: Text -> Maybe PopUpLink
parsePopUpLink = parseMaybe archiverLink
  where
    archiverLink :: Parser PopUpLink
    archiverLink = do
      _ <- chunk "return popUp('"
      url <- takeWhile1P Nothing (/= '\'')
      _ <- takeRest
      pure $ PopUpLink url

newtype AverageRating = AverageRating {unRating :: Float}
  deriving newtype (Show, Eq)

_AverageRating :: Prism' Text AverageRating
_AverageRating = prism' (pack . show . unRating) parseAverageRating

parseAverageRating :: Text -> Maybe AverageRating
parseAverageRating = parseMaybe averageRating
  where
    averageRating :: Parser AverageRating
    averageRating =
      ( do
          _ <- chunk "Average: "
          AverageRating <$> float
      )
        <|> (chunk "Not Yet Rated" >> pure (AverageRating 0))

newtype GalleryLength = GalleryLength {unGalleryLength :: Int}
  deriving newtype (Show, Eq)

_GalleryLength :: Prism' Text GalleryLength
_GalleryLength = prism' (pack . show . unGalleryLength) parseGalleryLength

parseGalleryLength :: Text -> Maybe GalleryLength
parseGalleryLength = parseMaybe galleryLength
  where
    galleryLength :: Parser GalleryLength
    galleryLength = do
      d <- decimal
      _ <- chunk " pages"
      pure $ GalleryLength d

newtype FavoriteCount = FavoriteCount {unFavoriteCount :: Int}
  deriving newtype (Show, Eq)

parseFavoriteCount :: Text -> Maybe FavoriteCount
parseFavoriteCount = parseMaybe favoriteCount
  where
    favoriteCount :: Parser FavoriteCount
    favoriteCount = do
      d <- decimal
      _ <- chunk " times"
      pure $ FavoriteCount d

data Gallery = Gallery
  { galleryId :: {-# UNPACK #-} !Int,
    token :: {-# UNPACK #-} !Text
  }
  deriving (Show, Eq)

_GalleryLink :: Prism' Text Gallery
_GalleryLink = prism' toGalleryLink parseGalleryLink

toGalleryLink :: Gallery -> Text
toGalleryLink Gallery {..} = "https://exhentai.org/g/" <> pack (show galleryId) <> "/" <> token <> "/"

toMpvLink :: Gallery -> Text
toMpvLink Gallery {..} = "https://exhentai.org/mpv/" <> pack (show galleryId) <> "/" <> token <> "/"

parseGalleryLink :: Text -> Maybe Gallery
parseGalleryLink = parseMaybe galleryLink
  where
    galleryLink :: Parser Gallery
    galleryLink = do
      _ <- chunk "https://exhentai.org/g/"
      galleryId <- decimal
      _ <- single '/'
      token <- takeWhile1P Nothing (/= '/')
      _ <- optional $ single '/'
      pure Gallery {..}

parsePreviewLink :: Text -> Maybe Text
parsePreviewLink = parseMaybe previewLink
  where
    previewLink :: Parser Text
    previewLink = do
      _ <- many $ do
        notFollowedBy urlOpening
        anySingle
      _ <- urlOpening
      url <- takeWhile1P Nothing (/= ')')
      _ <- takeRest
      pure url
    urlOpening :: Parser Text
    urlOpening = chunk "url("
