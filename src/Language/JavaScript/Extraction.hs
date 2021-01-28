{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module. Extract values from mpv script
module Language.JavaScript.Extraction where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe
import Data.Text (Text)
import Language.JavaScript.Parser.AST
import Optics.TH
import Text.Read
import Web.Exhentai.Utils hiding (div)
import Prelude hiding ((!!))

data MpvImage = MpvImage
  { name :: {-# UNPACK #-} Text,
    key :: {-# UNPACK #-} Text,
    thumbnail :: {-# UNPACK #-} Text
  }
  deriving (Show, Eq)

instance FromJSON MpvImage where
  parseJSON = withObject "mpv image" $ \o ->
    MpvImage
      <$> o .: "n"
      <*> o .: "k"
      <*> o .: "t"

-- | All the variables defined in the scripts that came with the MPV
data Vars = Vars
  { gid :: {-# UNPACK #-} Int,
    mpvkey :: {-# UNPACK #-} Text,
    pageCount :: {-# UNPACK #-} Int,
    imageList :: [MpvImage]
  }
  deriving (Show, Eq)

class As a b where
  as :: a -> Maybe b

instance As JSExpression Int where
  as (JSDecimal _ s) = decode (pack s)
  as (JSExpressionBinary expr1 op expr2) = do
    i1 <- as expr1
    i2 <- as expr2
    case op of
      JSBinOpPlus {} -> pure $ i1 + i2
      JSBinOpMinus {} -> pure $ i1 - i2
      JSBinOpTimes {} -> pure $ i1 * i2
      JSBinOpDivide {} -> pure $ i1 `div` i2
      _ -> Nothing
  as (JSUnaryExpression op expr) = do
    i <- as expr
    case op of
      JSUnaryOpIncr {} -> pure $ i + 1
      JSUnaryOpDecr {} -> pure $ i - 1
      JSUnaryOpMinus {} -> pure $ - i
      JSUnaryOpPlus {} -> pure i
      _ -> Nothing
  as (JSVarInitExpression _ initializer) = as initializer
  as _ = Nothing

instance As JSExpression a => As JSVarInitializer a where
  as (JSVarInit _ expr) = as expr
  as _ = Nothing

instance As (JSCommaList JSExpression) a => As JSStatement a where
  as (JSVariable _ l _) = as l
  as _ = Nothing

instance {-# OVERLAPPABLE #-} As a b => As (JSCommaList a) b where
  as (JSLOne x) = as x
  as (JSLCons _ _ x) = as x
  as _ = Nothing

instance As JSExpression Text where
  as (JSStringLiteral _ s) = decode (pack s)
  as (JSVarInitExpression _ initializer) = as initializer
  as _ = Nothing

instance As JSPropertyName Text where
  as (JSPropertyString _ s) = readMaybe s
  as _ = Nothing

instance As JSArrayElement a => As JSExpression [a] where
  as (JSArrayLiteral _ l _) = Just $ mapMaybe as l
  as (JSVarInitExpression _ initializer) = as initializer
  as _ = Nothing

instance As JSExpression a => As JSArrayElement a where
  as (JSArrayElement expr) = as expr
  as (JSArrayComma _) = Nothing

class AsPair a where
  asPair :: a -> Maybe (Text, Text)

instance AsPair JSObjectProperty where
  asPair (JSPropertyNameandValue n _ [expr]) = (,) <$> as n <*> as expr
  asPair _ = Nothing

instance As (JSCommaList JSObjectProperty) [(Text, Text)] where
  as (JSLCons xs _ p) = (:) <$> asPair p <*> as xs
  as (JSLOne p) = pure <$> asPair p
  as JSLNil = pure []

instance {-# OVERLAPPABLE #-} As (JSCommaList a) b => As (JSCommaTrailingList a) b where
  as (JSCTLComma l _) = as l
  as (JSCTLNone l) = as l

instance As JSObjectPropertyList MpvImage where
  as l = do
    mapping <- (as l :: Maybe [(Text, Text)])
    thumbnail <- lookup "t" mapping
    key <- lookup "k" mapping
    name <- lookup "n" mapping
    pure MpvImage {..}

instance As JSExpression MpvImage where
  as (JSObjectLiteral _ l _) = as l
  as _ = Nothing

instance As JSAST Vars where
  as (JSAstProgram stmts _) = do
    gidStmt <- stmts !! 1
    gid <- as gidStmt
    imgStmt <- stmts !! 3
    imageList <- as imgStmt
    keyStmt <- stmts !! 2
    mpvkey <- as keyStmt
    pgCountStmt <- stmts !! 9
    pageCount <- as pgCountStmt
    pure Vars {..}
  as _ = Nothing

makeFieldLabelsWith noPrefixFieldLabels ''Vars
makeFieldLabelsWith noPrefixFieldLabels ''MpvImage

{-
extractEnv :: Text -> IO (Result Vars)
extractEnv script = quickjs $ do
  eval_ $ encodeUtf8 script
  gid' <- eval "gid"
  mpvkey' <- eval "mpvkey"
  imageList' <- eval "imagelist"
  pageCount' <- eval "pagecount"
  pure $ do
    gid <- fromJSON gid'
    mpvkey <- fromJSON mpvkey'
    imageList <- fromJSON imageList'
    pageCount <- fromJSON pageCount'
    pure Vars {..}
-}
