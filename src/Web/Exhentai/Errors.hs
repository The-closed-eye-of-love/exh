{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Web.Exhentai.Errors where

import Control.Exception
import Data.Text (Text)

data ExhentaiError
  = JSONParseFailure String
  | XMLParseFailure Text Text
  | ExtractionFailure String
  deriving (Show, Eq)
  deriving (Exception)
