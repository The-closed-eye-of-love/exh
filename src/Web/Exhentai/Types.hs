{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Exhentai.Types where

import Control.Lens
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

data GalleryCat
  = Doujinshi
  | Manga
  | ArtistCG
  | GameCG
  | NonH
  | ImageSet
  | Western
  | Cosplay
  | Misc
  | Private
  deriving (Show, Eq, Enum)

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
    averageRating = do
      _ <- chunk "Average: "
      AverageRating <$> float

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
