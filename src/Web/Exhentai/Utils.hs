{-# LANGUAGE RankNTypes #-}

module Web.Exhentai.Utils where

import Control.Lens
import Data.Maybe (isNothing)
import Data.Text (Text)
import Text.XML
import Text.XML.Lens

attributeSatisfies' :: Name -> (Maybe Text -> Bool) -> Traversal' Element Element
attributeSatisfies' n p = filtered (p . preview (attrs . ix n))

withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute = flip attributeSatisfies' isNothing
