module Web.Exhentai.Parsing.Image where

import Control.Lens
import Data.Text (Text)
import Text.XML.Lens
import Web.Exhentai.Utils
import Prelude hiding (id)

imageSrc :: Traversal' Element Text
imageSrc = id "i1" ... id "i3" ... a ... img . attr "src"

nextPage :: Traversal' Element Text
nextPage = id "i1" ... id "i3" ... a . attr "href"
