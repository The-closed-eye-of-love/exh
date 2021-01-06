module Main where

import Conduit
import Control.Retry
import Data.Maybe
import Network.HTTP.Client
import Test.Hspec
import Text.HTML.DOM
import Web.Exhentai.API.Auth
import Web.Exhentai.API.Gallery
import Web.Exhentai.API.MPV
import qualified Web.Exhentai.Parsing.Gallery as G
import Web.Exhentai.Parsing.Image
import qualified Web.Exhentai.Parsing.Search as S
import Web.Exhentai.Types
import Web.Exhentai.Types.CookieT
import Web.Exhentai.Utils
import Prelude hiding (readFile)

main :: IO ()
main = do
  sanityCheck

sanityCheck :: IO ()
sanityCheck = do
  image <- readFile "test/Image.html"
  galleryMpv <- readFile "test/Gallery-MPV.html"
  galleryNonMpv <- readFile "test/Gallery-NonMPV.html"
  galleryReplaced <- readFile "test/Gallery-Replaced.html"
  search <- readFile "test/Search-Extended.html"
  hspec $ do
    describe "Image.imageSrc" $ do
      it "should return the image source link" $ do
        (image ^?: imageSrc) `shouldSatisfy` isJust

    describe "Image.nextPage" $ do
      it "should return the link to the next page" $ do
        (image ^?: nextPage) `shouldSatisfy` isJust

    describe "Gallery.enTitle" $ do
      it "should return the title of the gallery" $ do
        (galleryMpv ^?: G.enTitle) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.enTitle) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.enTitle) `shouldSatisfy` isJust

    describe "Gallery.jaTitle" $ do
      it "should return the original title of the gallery" $ do
        (galleryMpv ^?: G.jaTitle) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.jaTitle) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.jaTitle) `shouldSatisfy` isJust

    describe "Gallery.category" $ do
      it "should return the category of the gallery" $ do
        (galleryMpv ^?: G.category) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.category) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.category) `shouldSatisfy` isJust

    describe "Gallery.uploader" $ do
      it "should return the uploader of the gallery" $ do
        (galleryMpv ^?: G.uploader) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.uploader) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.uploader) `shouldSatisfy` isJust

    describe "Gallery.ratingCount" $ do
      it "should return the total rating count of the gallery" $ do
        (galleryMpv ^?: G.ratingCount) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.ratingCount) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.ratingCount) `shouldSatisfy` isJust

    describe "Gallery.averageRating" $ do
      it "should return the average rating of the gallery" $ do
        (galleryMpv ^?: G.averageRating) `shouldSatisfy` isJust
        (galleryNonMpv ^?: G.averageRating) `shouldSatisfy` isJust
        (galleryReplaced ^?: G.averageRating) `shouldSatisfy` isJust

    describe "Gallery.newer" $ do
      it "should return the link to the newer version of the library on replaced galleries and nothing otherwise" $ do
        (galleryMpv ^?: G.newer) `shouldSatisfy` isNothing
        (galleryNonMpv ^?: G.newer) `shouldSatisfy` isNothing
        (galleryReplaced ^?: G.newer) `shouldSatisfy` isJust

    describe "Gallery.parseGallery" $ do
      it "should parse gallery just fine" $ do
        parseGallery galleryMpv `shouldSatisfy` isJust
        parseGallery galleryNonMpv `shouldSatisfy` isJust
        parseGallery galleryReplaced `shouldSatisfy` isJust

    describe "Search.pages" $ do
      it "should return available page range" $ do
        (search ^?: S.pages) `shouldSatisfy` isJust

    describe "Search.galleryPreviewElement" $ do
      it "should return gallery preview elements" $ do
        (search ^?: S.galleryPreviewElement) `shouldSatisfy` isJust

    describe "Search.galleryLink" $ do
      it "should return galleries" $ do
        (search ^?: S.galleryPreviewElement . S.galleryLink) `shouldSatisfy` isJust
