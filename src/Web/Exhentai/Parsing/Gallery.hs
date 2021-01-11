{-# LANGUAGE RankNTypes #-}

module Web.Exhentai.Parsing.Gallery where

import Control.Lens
import Data.Text (Text, pack)
import GHC.Generics
import Text.XML hiding (readFile)
import Text.XML.Lens
import Web.Exhentai.Types
  ( AverageRating,
    Gallery,
    GalleryCat,
    PopUpLink,
    _AverageRating,
    _GalleryCat,
    _GalleryLink,
    _PopUpLink,
  )
import Web.Exhentai.Utils
import Prelude hiding (div, id, readFile)

data TagCategory
  = Language
  | Parody
  | Character
  | Group
  | Artist
  | Male
  | Female
  | Misc
  | Reclass
  deriving (Show, Eq, Enum, Generic)

readTagCat :: Text -> Maybe TagCategory
readTagCat "language:" = Just Language
readTagCat "parody:" = Just Parody
readTagCat "character:" = Just Character
readTagCat "group:" = Just Group
readTagCat "artist:" = Just Artist
readTagCat "male:" = Just Male
readTagCat "female:" = Just Female
readTagCat "misc:" = Just Misc
readTagCat "reclass:" = Just Reclass
readTagCat _ = Nothing

_TagCategory :: Prism' Text TagCategory
_TagCategory = prism' (pack . show) readTagCat

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

previewStr :: Traversal' Element Text
previewStr = meta ... id "gleft" ... id "gd1" ... attr "style"

category :: Traversal' Element GalleryCat
category = mainMetaL ... div . id "gdc" ... div . lower . _Content . _GalleryCat

uploader :: Traversal' Element Text
uploader = mainMetaL ... div . id "gdn" ... a . lower . _Content

metaPath :: Traversal' Element Element
metaPath = mainMetaL ... div . id "gdd" ... table ... tr

metaKeys :: Traversal' Element Text
metaKeys = metaPath ... cl "gdt1" . lower . _Content

metaValues :: Traversal' Element Element
metaValues = metaPath ... cl "gdt2"

parent :: Traversal' Element Gallery
parent = lower . _Element . attr "href" . _GalleryLink

ratingCount :: Traversal' Element Int
ratingCount =
  mainMetaL ... id "gdr" ... table ... tr ... id "grt3" ... lower . _Content . viaShowRead

averageRating :: Traversal' Element AverageRating
averageRating =
  mainMetaL ... id "gdr" ... table ... tr ... id "rating_label" . lower . _Content . _AverageRating

tagList :: Traversal' Element Element
tagList = mainMetaM ... div . id "taglist" ... table ... tr

tagCategory :: Traversal' Element TagCategory
tagCategory = tagList ... td . cl "tc" . lower . _Content . _TagCategory

tagsByCategory :: Traversal' Element Element
tagsByCategory = tagList ... td . withoutAttribute "class"

tags :: Traversal' Element Text
tags = td ... div ... a . lower . _Content

popupLink :: Traversal' Element PopUpLink
popupLink = mainMetaR ... named "p" ... attr "onclick" . _PopUpLink

imagePages :: Traversal' Element Text
imagePages = div . id "gdt" ... cl "gdtl" ... a . attr "href"

newer :: Traversal' Element Gallery
newer = div . id "gnd" ... a . attr "href" . _GalleryLink
