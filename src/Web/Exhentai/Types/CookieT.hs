{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Exhentai.Types.CookieT where

import Conduit
import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Time
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Function ((&))
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit

class (Monad m, MonadCatch m, MonadThrow m) => MonadHttp m where
  formRequest :: String -> m Request
  attachFormData :: [Part] -> Request -> m Request
  sendRequest :: Request -> m (Response (ConduitT () ByteString (ResourceT m) ()))
  sendRequestNoBody :: Request -> m (Response ())

class (MonadMask m, MonadTime m, MonadHttp m) => MonadHttpState m where
  takeCookieJar :: m CookieJar
  readCookieJar :: m CookieJar
  putCookieJar :: CookieJar -> m ()

withJar :: MonadHttpState m => (Request -> m (Response b)) -> Request -> m (Response b)
withJar k req =
  bracketOnError
    takeCookieJar
    putCookieJar
    $ \jar -> do
      let req' = req {cookieJar = Just jar}
      resp <- k req'
      putCookieJar $ responseCookieJar resp
      pure resp

sendRequestWithJar :: MonadHttpState m => Request -> m (Response (ConduitT () ByteString (ResourceT m) ()))
sendRequestWithJar = withJar sendRequest

sendRequestNoBodyWithJar :: MonadHttpState m => Request -> m (Response ())
sendRequestNoBodyWithJar = withJar sendRequestNoBody

sendRequestWithJar' :: MonadHttpState m => Request -> m (Response (ConduitT () ByteString (ResourceT m) ()))
sendRequestWithJar' req = do
  jar <- readCookieJar
  let req' = req {cookieJar = Just jar}
  sendRequest req'

newtype CookieT m a = CookieT {unCookieT :: ReaderT (MVar CookieJar) (ReaderT Manager m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadIO,
      MonadResource,
      MonadUnliftIO,
      MonadError e,
      MonadBase b,
      MonadBaseControl b,
      MonadTime
    )

runCookieT :: MonadIO m => CookieT m a -> m a
runCookieT m = do
  manager <- liftIO newTlsManager
  ref <- liftIO $ newMVar mempty
  m
    & unCookieT
    & flip runReaderT ref
    & flip runReaderT manager

instance MonadTrans CookieT where
  lift = CookieT . lift . lift

instance
  ( MonadIO m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadThrow m
  ) =>
  MonadHttp (CookieT m)
  where
  formRequest = parseRequest
  attachFormData = formDataBody
  sendRequest req = do
    manager <- CookieT $ lift ask
    runResourceT $ http req manager
  sendRequestNoBody req = do
    manager <- CookieT $ lift ask
    liftIO $ httpNoBody req manager

instance
  ( MonadMask m,
    MonadUnliftIO m,
    MonadTime m
  ) =>
  MonadHttpState (CookieT m)
  where
  takeCookieJar = do
    ref <- CookieT ask
    liftIO $ takeMVar ref
  putCookieJar jar = do
    ref <- CookieT ask
    liftIO $ putMVar ref jar
  readCookieJar = do
    ref <- CookieT ask
    liftIO $ readMVar ref
