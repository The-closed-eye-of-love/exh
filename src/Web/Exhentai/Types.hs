{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Exhentai.Types where

import Control.Lens
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text

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
