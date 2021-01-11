{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Web.Exhentai.Errors where

import Control.Exception
import Data.Text (Text)
import GHC.Generics

data ExhentaiError
  = JSONParseFailure String
  | XMLParseFailure
      { reason :: Text,
        url :: Text
      }
  | ExtractionFailure String
  deriving (Show, Eq, Generic)
  deriving (Exception)
