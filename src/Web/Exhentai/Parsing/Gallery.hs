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
{-# INLINE meta #-}

title :: Traversal' Element Element
title = meta .// (div % id "gd2")
{-# INLINE title #-}

enTitle :: Traversal' Element Text
enTitle = title .// (h1 % id "gn" <% lower % _Content)
{-# INLINE enTitle #-}

jaTitle :: Traversal' Element Text
jaTitle = title .// (h1 % id "gj" <% lower % _Content)
{-# INLINE jaTitle #-}

mainMeta :: Traversal' Element Element
mainMeta = meta .// (div % id "gmid")
{-# INLINE mainMeta #-}

mainMetaL :: Traversal' Element Element
mainMetaL = mainMeta .// (div % id "gd3")
{-# INLINE mainMetaL #-}

mainMetaM :: Traversal' Element Element
mainMetaM = mainMeta .// (div % id "gd4")
{-# INLINE mainMetaM #-}

mainMetaR :: Traversal' Element Element
mainMetaR = mainMeta .// (div % id "gd5")
{-# INLINE mainMetaR #-}

previewStr :: Traversal' Element Text
previewStr = meta .// id "gleft" .// id "gd1" .// attr "style"
{-# INLINE previewStr #-}

category :: Traversal' Element Text
category = mainMetaL .// (div % id "gdc") .// (div <% lower % _Content)
{-# INLINE category #-}

uploader :: Traversal' Element Text
uploader = mainMetaL .// (div % id "gdn") .// (a % lower %> _Content)
{-# INLINE uploader #-}

metaPath :: Traversal' Element Element
metaPath = mainMetaL .// (div % id "gdd") .// table .// tr
{-# INLINE metaPath #-}

metaKeys :: Traversal' Element Text
metaKeys = metaPath .// (cl "gdt1" <% lower % _Content)
{-# INLINE metaKeys #-}

metaValues :: Traversal' Element Element
metaValues = metaPath .// cl "gdt2"
{-# INLINE metaValues #-}

parent :: Traversal' Element Text
parent = lower %> _Element % attr "href"
{-# INLINE parent #-}

ratingCount :: Traversal' Element Int
ratingCount =
  mainMetaL .// id "gdr" .// table .// tr .// id "grt3" .// (lower %> _Content % viaShowRead)
{-# INLINE ratingCount #-}

averageRating :: Traversal' Element Text
averageRating =
  mainMetaL .// id "gdr" .// table .// tr .// (id "rating_label" % lower %> _Content)
{-# INLINE averageRating #-}

tagList :: Traversal' Element Element
tagList = mainMetaM .// (div % id "taglist") .// table .// tr
{-# INLINE tagList #-}

tagCategory :: Traversal' Element Text
tagCategory = tagList .// (td % cl "tc" % lower %> _Content)
{-# INLINE tagCategory #-}

tagsByCategory :: Traversal' Element Element
tagsByCategory = tagList .// (td % withoutAttribute "class")
{-# INLINE tagsByCategory #-}

tags :: Traversal' Element Text
tags = td .// div .// (a % lower %> _Content)
{-# INLINE tags #-}

popupLink :: Traversal' Element Text
popupLink = mainMetaR .// named "p" .// attr "onclick"
{-# INLINE popupLink #-}

imagePages :: Traversal' Element Text
imagePages = (div % id "gdt") .// cl "gdtl" .// (a % attr "href")
{-# INLINE imagePages #-}

newer :: Traversal' Element Text
newer = (div % id "gnd") .// (a % attr "href")
{-# INLINE newer #-}
