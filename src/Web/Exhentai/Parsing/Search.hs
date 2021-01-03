module Web.Exhentai.Parsing.Search where

import Control.Lens
import Data.Text (Text)
import Text.XML.Lens
import Web.Exhentai.Types
import Web.Exhentai.Utils
import Prelude hiding (div)

pages :: Traversal' Element Int
pages = cl "ido" . deepen . div . deepen . cl "ptt" . deepen . tr . deepen . td . deepen . a . lower . _Content . viaShowRead

galleryPreview :: Traversal' Element Element
galleryPreview = cl "ido" . deepen . div . deepen . cl "itg gltc" . deepen . tr

title :: Traversal' Element Text
title = cl "gl3c glname" . deepen . a . deepen . cl "glink" . lower . _Content

tags :: Traversal' Element Text
tags = cl "gl3c glname" . deepen . a . deepen . div . deepen . cl "gt" . attr "title"

galleryLink :: Traversal' Element Text
galleryLink = cl "gl3c glname" . deepen . a . attr "href"

uploader :: Traversal' Element Text
uploader = cl "gl4c glhide" . deepen . div . deepen . a . lower . _Content

glength :: Traversal' Element GalleryLength
glength = cl "gl4c glhide" . deepen . div . lower . _Content . _GalleryLength
