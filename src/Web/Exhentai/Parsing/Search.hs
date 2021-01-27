-- | Internal modules
module Web.Exhentai.Parsing.Search where

import Data.Text (Text)
import Optics.Core
import Text.XML.Optics
import Web.Exhentai.Utils
import Prelude hiding (div)

pages :: Traversal' Element Int
pages = pagesElem .// (a % lower %> _Content % viaShowRead)
{-# INLINE pages #-}

pagesElem :: Traversal' Element Element
pagesElem = cl "ido" .// div .// cl "ptt" .// tr .// td
{-# INLINE pagesElem #-}

linkOf :: Traversal' Element Text
linkOf = lower %> _Element % attr "href"
{-# INLINE linkOf #-}

galleryPreviewElement :: Traversal' Element Element
galleryPreviewElement = cl "ido" .// div .// cl "itg glte" .// tr
{-# INLINE galleryPreviewElement #-}

previewImage :: Traversal' Element Text
previewImage = tr .// cl "gl1e" .// div .// a .// (img % attr "src")
{-# INLINE previewImage #-}

title :: Traversal' Element Text
title = tr .// cl "gl1e" .// div .// a .// (img % attr "title")
{-# INLINE title #-}

galleryLink :: Traversal' Element Text
galleryLink = tr .// cl "gl1e" .// div .// (a % attr "href")
{-# INLINE galleryLink #-}
