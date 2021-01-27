{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Exhentai.API.Auth
  ( Credential (..),
    auth,
  )
where

import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Exh
import Data.ByteString (ByteString)
import GHC.Generics
import Network.HTTP.Client hiding (Cookie)
import Network.HTTP.Client.MultipartFormData
import Optics.TH

data Credential = Credential
  { username :: ByteString,
    password :: ByteString
  }
  deriving (Show, Eq, Generic)

-- | Authenticates and loads user preferences.
-- This should be called before any other functions are called
auth :: Effs '[Http, Error HttpException, Cookie, ConduitIO, Bracket] m => Credential -> m ()
auth Credential {..} = do
  initReq <- formRequest "POST https://forums.e-hentai.org/index.php"
  let parts =
        [ partBS "CookieDate" "1",
          partBS "b" "d",
          partBS "bt" "1-6",
          partBS "UserName" username,
          partBS "PassWord" password,
          partBS "ipb_login_submit" "Login!"
        ]
  let req =
        setQueryString
          [ ("act", Just "Login"),
            ("CODE", Just "01")
          ]
          initReq
  finalReq <- attachFormData parts req
  modifyJar finalReq
  req2 <- formRequest "https://exhentai.org"
  modifyJar req2
  req3 <- formRequest "https://exhentai.org/uconfig.php"
  modifyJar req3
  req4 <- formRequest "https://exhentai.org/mytags"
  modifyJar req4

makeFieldLabelsWith noPrefixFieldLabels ''Credential
