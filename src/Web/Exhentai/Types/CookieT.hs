{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Retry
import Data.ByteString (ByteString)
import Data.Function ((&))
import GHC.Generics
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS

newtype Policy = Policy RetryPolicy

class (Monad m, MonadCatch m, MonadThrow m) => MonadHttp m where
  getRetryPolicy :: m Policy
  formRequest :: String -> m Request
  attachFormData :: [Part] -> Request -> m Request
  respOpen :: MonadIO n => Request -> m (Response (ConduitT i ByteString n ()))
  respClose :: Response body -> m ()
  reqNoBody :: Request -> m (Response ())

class (MonadMask m, MonadTime m, MonadHttp m, MonadUnliftIO m) => MonadHttpState m where
  takeCookieJar :: m CookieJar
  readCookieJar :: m CookieJar
  putCookieJar :: CookieJar -> m ()

modifyingJar :: MonadHttpState m => Request -> m ()
modifyingJar req =
  bracketOnError
    takeCookieJar
    putCookieJar
    $ \jar -> do
      let req' = req {cookieJar = Just jar}
      resp <- retryWhenTimeout $ reqNoBody req'
      putCookieJar $ responseCookieJar resp
      pure ()

openWithJar :: (MonadHttpState m, MonadIO n) => Request -> m (Response (ConduitT i ByteString n ()))
openWithJar req = do
  jar <- readCookieJar
  let req' = req {cookieJar = Just jar}
  respOpen req'

withJar :: (MonadHttpState m, MonadIO n) => Request -> (ConduitT i ByteString n () -> m a) -> m a
withJar req k = do
  jar <- readCookieJar
  let req' = req {cookieJar = Just jar}
  bracket
    (respOpen req')
    respClose
    (k . responseBody)

data CookieEnv = CookieEnv
  { policy :: Policy,
    jarRef :: MVar CookieJar,
    manager :: {-# UNPACK #-} !Manager
  }
  deriving (Generic)

instance HasHttpManager CookieEnv where
  getHttpManager = manager

newtype CookieT m a = CookieT {unCookieT :: ReaderT CookieEnv m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader CookieEnv,
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

runCookieT :: MonadIO m => RetryPolicy -> CookieT m a -> m a
runCookieT (Policy -> policy) m = do
  manager <- liftIO newTlsManager
  jarRef <- liftIO $ newMVar mempty
  m
    & unCookieT
    & flip runReaderT CookieEnv {..}

instance MonadTrans CookieT where
  lift = CookieT . lift

instance
  ( MonadIO m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadThrow m
  ) =>
  MonadHttp (CookieT m)
  where
  getRetryPolicy = asks policy
  formRequest = parseRequest
  attachFormData = formDataBody
  respOpen = responseOpen
  respClose = responseClose
  reqNoBody = httpNoBody

retryWhenTimeout :: MonadHttpState m => m a -> m a
retryWhenTimeout action = do
  Policy policy <- getRetryPolicy
  recovering policy handlers (const action)
  where
    handlers =
      skipAsyncExceptions
        ++ [ const (Handler (pure . judge))
           ]
    judge (HttpExceptionRequest _ c)
      | ResponseTimeout <- c = True
      | ConnectionTimeout <- c = True
    judge _ = False

instance
  ( MonadMask m,
    MonadUnliftIO m,
    MonadTime m
  ) =>
  MonadHttpState (CookieT m)
  where
  takeCookieJar = do
    ref <- asks jarRef
    liftIO $ takeMVar ref
  putCookieJar jar = do
    ref <- asks jarRef
    liftIO $ putMVar ref jar
  readCookieJar = do
    ref <- asks jarRef
    liftIO $ readMVar ref
