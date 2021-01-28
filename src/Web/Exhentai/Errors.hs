{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Web.Exhentai.Errors where

import Control.Exception
import Data.Text (Text)

data ExhentaiError
  = JSONParseFailure String
  | XMLParseFailure
      { reason :: Text,
        url :: Text
      }
  | ExtractionFailure
  deriving (Show, Eq)
  deriving (Exception)
