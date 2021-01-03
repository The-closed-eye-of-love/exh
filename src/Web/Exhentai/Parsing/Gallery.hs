{-# LANGUAGE RankNTypes #-}

module Web.Exhentai.Parsing.Gallery where

import Control.Lens
import Data.Text (Text)
import Text.HTML.DOM
import Text.XML hiding (readFile)
import Text.XML.Lens
import Web.Exhentai.Types
import Web.Exhentai.Utils
import Prelude hiding (div, id, readFile)

scripts :: Traversal' Element Element
scripts = named "script" . attributeIs "type" "text/javascript"

meta :: Traversal' Element Element
meta = div . cl "gm"

title :: Traversal' Element Element
title = meta ... div . id "gd2"

enTitle :: Traversal' Element Text
enTitle = title ... h1 . id "gn" . lower . _Content

jaTitle :: Traversal' Element Text
jaTitle = title ... h1 . id "gj" . lower . _Content

mainMeta :: Traversal' Element Element
mainMeta = meta ... div . id "gmid"

mainMetaL :: Traversal' Element Element
mainMetaL = mainMeta ... div . id "gd3"

mainMetaM :: Traversal' Element Element
mainMetaM = mainMeta ... div . id "gd4"

mainMetaR :: Traversal' Element Element
mainMetaR = mainMeta ... div . id "gd5"

category :: Traversal' Element GalleryCat
category = mainMetaL ... div . id "gdc" ... div . lower . _Content . _GalleryCat

uploader :: Traversal' Element Text
uploader = mainMetaL ... div . id "gdn" ... a . lower . _Content

metaPath :: Traversal' Element Element
metaPath = mainMetaL ... div . id "gdd" ... table ... tr

metaKeys :: Traversal' Element Text
metaKeys = metaPath ... cl "gdt1" . lower . _Content

metaValues :: Traversal' Element Text
metaValues = metaPath ... cl "gdt2" . lower . _Content

ratingCount :: Traversal' Element Int
ratingCount =
  mainMetaL ... id "gdr" ... table ... tr ... id "grt3" ... lower . _Content . viaShowRead

averageRating :: Traversal' Element AverageRating
averageRating =
  mainMetaL ... id "gdr" ... table ... tr ... id "rating_label" . lower . _Content . _AverageRating

tagList :: Traversal' Element Element
tagList = mainMetaM ... div . id "taglist" ... table ... tr

tagCategory :: Traversal' Element Text
tagCategory = tagList ... td . cl "tc" . lower . _Content

tagsByCategory :: Traversal' Element Element
tagsByCategory = tagList ... td . withoutAttribute "class"

tags :: Traversal' Element Text
tags = td ... div ... a . lower . _Content

archiverLink :: Traversal' Element PopUpLink
archiverLink = mainMetaR ... cl "g2 gsp" ... attr "onclick" . _PopUpLink

torrentLink :: Traversal' Element PopUpLink
torrentLink = mainMetaR ... cl "g2" ... attr "onclick" . _PopUpLink

imagePages :: Traversal' Element Text
imagePages = div . id "gdt" ... cl "gdtl" ... a . attr "href"
