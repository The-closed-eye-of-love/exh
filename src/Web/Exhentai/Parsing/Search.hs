module Web.Exhentai.Parsing.Search where

import Control.Lens
import Data.Text (Text)
import Text.XML.Lens
import Web.Exhentai.Types
import Web.Exhentai.Utils
import Prelude hiding (div)

pages :: Traversal' Element Int
pages = pagesElem ... a . lower . _Content . viaShowRead

pagesElem :: Traversal' Element Element
pagesElem = cl "ido" ... div ... cl "ptt" ... tr ... td

linkOf :: Traversal' Element Text
linkOf = lower . _Element . attr "href"

galleryPreviewElement :: Traversal' Element Element
galleryPreviewElement = cl "ido" ... div ... cl "itg glte" ... tr

previewImage :: Traversal' Element Text
previewImage = tr ... cl "gl1e" ... div ... a ... img . attr "src"

title :: Traversal' Element Text
title = tr ... cl "gl1e" ... div ... a ... img . attr "title"

galleryLink :: Traversal' Element Gallery
galleryLink = tr ... cl "gl1e" ... div ... a . attr "href" . _GalleryLink

galleryLength :: Traversal' Element GalleryLength
galleryLength = tr ... cl "gl2e" ... div ... cl "gl3e" ... lower . _Content . _GalleryLength
