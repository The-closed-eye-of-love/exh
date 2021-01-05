{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Web.Exhentai.Errors where

import Control.Exception

data ExhentaiError
  = JSONParseFailure String
  | XMLParseFailure
  | ExtractionFailure String
  deriving (Show, Eq)
  deriving (Exception)
