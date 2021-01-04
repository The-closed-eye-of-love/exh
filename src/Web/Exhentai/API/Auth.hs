module Web.Exhentai.API.Auth where

import Data.ByteString
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Web.Exhentai.Types.CookieT

data Credential = Credential
  { username :: ByteString,
    password :: ByteString
  }
  deriving (Show, Eq)

auth :: MonadHttpState m => Credential -> m ()
auth Credential {..} = do
  let initReq = parseRequest_ "POST https://forums.e-hentai.org/index.php"
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
  _ <- sendRequestNoBodyWithJar finalReq
  _ <- sendRequestNoBodyWithJar $ parseRequest_ "https://exhentai.org"
  _ <- sendRequestNoBodyWithJar $ parseRequest_ "https://exhentai.org/uconfig.php"
  _ <- sendRequestNoBodyWithJar $ parseRequest_ "https://exhentai.org/mytags"
  pure ()
