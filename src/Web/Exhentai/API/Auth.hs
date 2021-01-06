module Web.Exhentai.API.Auth
  ( Credential (..),
    auth,
  )
where

import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.MultipartFormData
import Web.Exhentai.Types.CookieT

data Credential = Credential
  { username :: ByteString,
    password :: ByteString
  }
  deriving (Show, Eq)

auth :: MonadHttpState m => Credential -> m ()
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
  modifyingJar finalReq
  req2 <- formRequest "https://exhentai.org"
  modifyingJar req2
  req3 <- formRequest "https://exhentai.org/uconfig.php"
  modifyingJar req3
  req4 <- formRequest "https://exhentai.org/mytags"
  modifyingJar req4
  pure ()
