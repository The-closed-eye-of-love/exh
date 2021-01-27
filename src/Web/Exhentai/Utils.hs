{-# LANGUAGE RankNTypes #-}

module Web.Exhentai.Utils where

import Conduit
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Control.Monad.Trans.Cont
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Time
import Network.HTTP.Client hiding (Cookie)
import Optics.Core
import Text.HTML.DOM
import Text.Read (readMaybe)
import Text.XML hiding (sinkDoc)
import Text.XML.Optics
import Prelude hiding ((!!))

body :: Traversal' Document Element
body = (root % named "html") .// named "body"
{-# INLINE body #-}

div :: AffineTraversal' Element Element
div = named "div"
{-# INLINE div #-}

h1 :: AffineTraversal' Element Element
h1 = named "h1"
{-# INLINE h1 #-}

a :: AffineTraversal' Element Element
a = named "a"
{-# INLINE a #-}

table :: AffineTraversal' Element Element
table = named "table"
{-# INLINE table #-}

tr :: AffineTraversal' Element Element
tr = named "tr"
{-# INLINE tr #-}

td :: AffineTraversal' Element Element
td = named "td"
{-# INLINE td #-}

img :: AffineTraversal' Element Element
img = named "img"
{-# INLINE img #-}

cl :: Text -> AffineTraversal' Element Element
cl = attributeIs "class"
{-# INLINE cl #-}

id :: Text -> AffineTraversal' Element Element
id = attributeIs "id"
{-# INLINE id #-}

viaShowRead :: (Show a, Read a) => Prism' Text a
viaShowRead = prism' (pack . show) (readMaybe . unpack)
{-# INLINE viaShowRead #-}

scripts :: AffineTraversal' Element Element
scripts = named "script" % attributeIs "type" "text/javascript"
{-# INLINE scripts #-}

infixl 8 ^?:

(^?:) :: (Is (Join A_Traversal l) A_Fold, Is l (Join A_Traversal l), Is A_Traversal (Join A_Traversal l)) => Document -> Optic l is Element Element a a -> Maybe a
doc ^?: fld = doc ^? pre (body .// fld)
{-# INLINE (^?:) #-}

infixl 8 ^..:

(^..:) :: (Is (Join A_Traversal l) A_Fold, Is l (Join A_Traversal l), Is A_Traversal (Join A_Traversal l)) => Document -> Optic l is Element Element a a -> [a]
doc ^..: fld = doc ^.. body .// fld
{-# INLINE (^..:) #-}

sinkAeson :: (FromJSON a, Monad m) => ConduitT ByteString o m (Either String a)
sinkAeson = eitherDecode <$> sinkLazy

jsonRequest :: (FromJSON a, Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m) => Request -> m (Either String a)
jsonRequest req = evalContT $ do
  resp <- withSource req
  lift $ runConduitIO $ responseBody resp .| sinkAeson

htmlRequest :: Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m => Request -> m Document
htmlRequest req = evalContT $ do
  resp <- withSource req
  lift $ runConduitIO $ responseBody resp .| sinkDoc

htmlRequest' :: Effs '[Http, Error HttpException, ConduitIO, Cookie, Bracket] m => Text -> m Document
htmlRequest' url = do
  req <- formRequest $ unpack url
  htmlRequest req

parseUploadTime :: Text -> Maybe UTCTime
parseUploadTime s = parseTimeM True defaultTimeLocale "%F %R" $ unpack s

annotate :: ann -> Maybe a -> Either ann a
annotate _ (Just a') = Right a'
annotate ann Nothing = Left ann

(!!) :: [a] -> Int -> Maybe a
l !! i
  | i == 0,
    (x : _) <- l =
    Just x
  | i > 0,
    (_ : xs) <- l =
    xs !! (i - 1)
  | otherwise = Nothing
{-# INLINE (!!) #-}
