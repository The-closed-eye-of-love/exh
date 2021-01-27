-- | Internal modules
module Web.Exhentai.Parsing.Search where

import Data.Text (Text)
import Optics.Core
import Text.XML.Optics
import Web.Exhentai.Utils
import Prelude hiding (div)

pages :: Traversal' Element Int
pages = pagesElem .// (a % lower %> _Content % viaShowRead)

pagesElem :: Traversal' Element Element
pagesElem = cl "ido" .// div .// cl "ptt" .// tr .// td

linkOf :: Traversal' Element Text
linkOf = lower %> _Element % attr "href"

galleryPreviewElement :: Traversal' Element Element
galleryPreviewElement = cl "ido" .// div .// cl "itg glte" .// tr

previewImage :: Traversal' Element Text
previewImage = tr .// cl "gl1e" .// div .// a .// (img % attr "src")

title :: Traversal' Element Text
title = tr .// cl "gl1e" .// div .// a .// (img % attr "title")

galleryLink :: Traversal' Element Text
galleryLink = tr .// cl "gl1e" .// div .// (a % attr "href")
