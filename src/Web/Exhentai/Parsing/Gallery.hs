{-# LANGUAGE RankNTypes #-}

-- | Internal module
module Web.Exhentai.Parsing.Gallery where

import Data.Text (Text)
import Optics.Core
import Text.XML hiding (readFile)
import Text.XML.Optics
import Web.Exhentai.Utils
import Prelude hiding (div, id, readFile)

meta :: AffineTraversal' Element Element
meta = div % cl "gm"

title :: Traversal' Element Element
title = meta .// (div % id "gd2")

enTitle :: Traversal' Element Text
enTitle = title .// (h1 % id "gn" <% lower % _Content)

jaTitle :: Traversal' Element Text
jaTitle = title .// (h1 % id "gj" <% lower % _Content)

mainMeta :: Traversal' Element Element
mainMeta = meta .// (div % id "gmid")

mainMetaL :: Traversal' Element Element
mainMetaL = mainMeta .// (div % id "gd3")

mainMetaM :: Traversal' Element Element
mainMetaM = mainMeta .// (div % id "gd4")

mainMetaR :: Traversal' Element Element
mainMetaR = mainMeta .// (div % id "gd5")

previewStr :: Traversal' Element Text
previewStr = meta .// id "gleft" .// id "gd1" .// attr "style"

category :: Traversal' Element Text
category = mainMetaL .// (div % id "gdc") .// (div <% lower % _Content)

uploader :: Traversal' Element Text
uploader = mainMetaL .// (div % id "gdn") .// (a % lower %> _Content)

metaPath :: Traversal' Element Element
metaPath = mainMetaL .// (div % id "gdd") .// table .// tr

metaKeys :: Traversal' Element Text
metaKeys = metaPath .// (cl "gdt1" <% lower % _Content)

metaValues :: Traversal' Element Element
metaValues = metaPath .// cl "gdt2"

parent :: Traversal' Element Text
parent = lower %> _Element % attr "href"

ratingCount :: Traversal' Element Int
ratingCount =
  mainMetaL .// id "gdr" .// table .// tr .// id "grt3" .// (lower %> _Content % viaShowRead)

averageRating :: Traversal' Element Text
averageRating =
  mainMetaL .// id "gdr" .// table .// tr .// (id "rating_label" % lower %> _Content)

tagList :: Traversal' Element Element
tagList = mainMetaM .// (div % id "taglist") .// table .// tr

tagCategory :: Traversal' Element Text
tagCategory = tagList .// (td % cl "tc" % lower %> _Content)

tagsByCategory :: Traversal' Element Element
tagsByCategory = tagList .// (td % withoutAttribute "class")

tags :: Traversal' Element Text
tags = td .// div .// (a % lower %> _Content)

popupLink :: Traversal' Element Text
popupLink = mainMetaR .// named "p" .// attr "onclick"

imagePages :: Traversal' Element Text
imagePages = (div % id "gdt") .// cl "gdtl" .// (a % attr "href")

newer :: Traversal' Element Text
newer = (div % id "gnd") .// (a % attr "href")
