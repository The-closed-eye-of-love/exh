{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Exhentai.Types where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Void
import Optics.TH
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

parsePopUpLink :: Text -> Maybe Text
parsePopUpLink = parseMaybe archiverLink
  where
    archiverLink :: Parser Text
    archiverLink = do
      _ <- chunk "return popUp('"
      url <- takeWhile1P Nothing (/= '\'')
      _ <- takeRest
      pure url

parseAverageRating :: Text -> Maybe Double
parseAverageRating = parseMaybe averageRating
  where
    averageRating :: Parser Double
    averageRating =
      ( do
          _ <- chunk "Average: "
          float
      )
        <|> (chunk "Not Yet Rated" >> pure 0)

parseGalleryLength :: Text -> Maybe Int
parseGalleryLength = parseMaybe galleryLength
  where
    galleryLength :: Parser Int
    galleryLength = do
      d <- decimal
      _ <- chunk " pages"
      pure d

parseFavoriteCount :: Text -> Maybe Int
parseFavoriteCount = parseMaybe favoriteCount
  where
    once = do
      _ <- chunk "Once"
      pure 1
    never = do
      _ <- chunk "Never"
      pure 0
    favoriteCount :: Parser Int
    favoriteCount =
      ( do
          d <- decimal
          _ <- chunk " times"
          pure d
      )
        <|> once
        <|> never

data Gallery = Gallery
  { galleryId :: {-# UNPACK #-} !Int,
    token :: {-# UNPACK #-} !Text
  }
  deriving (Show, Eq)

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

makeFieldLabelsWith noPrefixFieldLabels ''Gallery
