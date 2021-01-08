{-# LANGUAGE RankNTypes #-}

module Web.Exhentai.Utils where

import Conduit
import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Text (Text, pack, unpack)
import Data.Time
import Network.HTTP.Client.Conduit
import Text.HTML.DOM
import Text.Read
import Text.XML hiding (sinkDoc)
import Text.XML.Lens
import Web.Exhentai.Types.CookieT

attributeSatisfies' :: Name -> (Maybe Text -> Bool) -> Traversal' Element Element
attributeSatisfies' n p = filtered (p . preview (attrs . ix n))

withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute = flip attributeSatisfies' isNothing

lower :: Traversal' Element Node
lower = nodes . traverse

body :: Traversal' Document Element
body = root . named "html" ... named "body"

div :: Traversal' Element Element
div = named "div"

h1 :: Traversal' Element Element
h1 = named "h1"

a :: Traversal' Element Element
a = named "a"

table :: Traversal' Element Element
table = named "table"

tr :: Traversal' Element Element
tr = named "tr"

td :: Traversal' Element Element
td = named "td"

img :: Traversal' Element Element
img = named "img"

cl :: Text -> Traversal' Element Element
cl = attributeIs "class"

id :: Text -> Traversal' Element Element
id = attributeIs "id"

viaShowRead :: (Show a, Read a) => Prism' Text a
viaShowRead = prism' (pack . show) (readMaybe . unpack)

scripts :: Traversal' Element Element
scripts = named "script" . attributeIs "type" "text/javascript"

infixl 8 ^?:

(^?:) :: Document -> Fold Element a -> Maybe a
doc ^?: fld = doc ^? body ... fld

infixl 8 ^..:

(^..:) :: Document -> Fold Element a -> [a]
doc ^..: fld = doc ^.. body ... fld

sinkAeson :: (FromJSON a, Monad m) => ConduitT ByteString o m (Either String a)
sinkAeson = eitherDecode <$> sinkLazy

jsonRequest :: (FromJSON a, MonadHttpState m) => Request -> m (Either String a)
jsonRequest req = withJar req $ \source -> runConduit $ source .| sinkAeson

htmlRequest :: MonadHttpState m => Request -> m Document
htmlRequest req = withJar req $ \source -> runConduit $ source .| sinkDoc

htmlRequest' :: MonadHttpState m => Text -> m Document
htmlRequest' url = do
  req <- formRequest $ unpack url
  htmlRequest req

parseUploadTime :: Text -> Maybe UTCTime
parseUploadTime s = parseTimeM True defaultTimeLocale "%F %R" $ unpack s
