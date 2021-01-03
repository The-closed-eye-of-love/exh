module Web.Exhentai.Parsing.Search where

import Control.Lens
import Data.Text (Text)
import Text.XML.Lens
import Web.Exhentai.Types
import Web.Exhentai.Utils
import Prelude hiding (div)

pages :: Traversal' Element Int
pages = cl "ido" ... div ... cl "ptt" ... tr ... td ... a . lower . _Content . viaShowRead

galleryPreview :: Traversal' Element Element
galleryPreview = cl "ido" ... div ... cl "itg gltc" ... tr

title :: Traversal' Element Text
title = cl "gl3c glname" ... a ... cl "glink" . lower . _Content

tags :: Traversal' Element Text
tags = cl "gl3c glname" ... a ... div ... cl "gt" . attr "title"

galleryLink :: Traversal' Element Text
galleryLink = cl "gl3c glname" ... a . attr "href"

uploader :: Traversal' Element Text
uploader = cl "gl4c glhide" ... div ... a . lower . _Content

glength :: Traversal' Element GalleryLength
glength = cl "gl4c glhide" ... div . lower . _Content . _GalleryLength
