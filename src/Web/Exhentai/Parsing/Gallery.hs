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
title = meta . deepen . div . id "gd2"

enTitle :: Traversal' Element Text
enTitle = title . deepen . h1 . id "gn" . lower . _Content

jaTitle :: Traversal' Element Text
jaTitle = title . deepen . h1 . id "gj" . lower . _Content

mainMeta :: Traversal' Element Element
mainMeta = meta . deepen . div . id "gmid"

mainMetaL :: Traversal' Element Element
mainMetaL = mainMeta . deepen . div . id "gd3"

mainMetaM :: Traversal' Element Element
mainMetaM = mainMeta . deepen . div . id "gd4"

mainMetaR :: Traversal' Element Element
mainMetaR = mainMeta . deepen . div . id "gd5"

category :: Traversal' Element Text
category = mainMetaL . deepen . div . id "gdc" . deepen . div . lower . _Content

uploader :: Traversal' Element Text
uploader = mainMetaL . deepen . div . id "gdn" . deepen . a . lower . _Content

metaPath :: Traversal' Element Element
metaPath = mainMetaL . deepen . div . id "gdd" . deepen . table . deepen . tr

metaKeys :: Traversal' Element Text
metaKeys = metaPath . deepen . cl "gdt1" . lower . _Content

metaValues :: Traversal' Element Text
metaValues = metaPath . deepen . cl "gdt2" . lower . _Content

ratingCount :: Traversal' Element Int
ratingCount =
  mainMetaL . deepen . id "gdr" . deepen . table . deepen . tr . deepen . id "grt3" . deepen . lower . _Content . viaShowRead

averageRating :: Traversal' Element AverageRating
averageRating =
  mainMetaL . deepen . id "gdr" . deepen . table . deepen . tr . deepen . id "rating_label" . lower . _Content . _AverageRating

tagList :: Traversal' Element Element
tagList = mainMetaM . deepen . div . id "taglist" . deepen . table . deepen . tr

tagCategory :: Traversal' Element Text
tagCategory = tagList . deepen . td . cl "tc" . lower . _Content

tagsByCategory :: Traversal' Element Element
tagsByCategory = tagList . deepen . td . withoutAttribute "class"

tags :: Traversal' Element Text
tags = deepen . div . deepen . a . lower . _Content

archiverLink :: Traversal' Element PopUpLink
archiverLink = mainMetaR . deepen . cl "g2 gsp" . deepen . attr "onclick" . _PopUpLink

torrentLink :: Traversal' Element PopUpLink
torrentLink = mainMetaR . deepen . cl "g2" . deepen . attr "onclick" . _PopUpLink

imagePages :: Traversal' Element Text
imagePages = div . id "gdt" . deepen . cl "gdtl" . deepen . a . attr "href"
