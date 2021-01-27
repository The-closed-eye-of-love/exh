{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module
module Control.Effect.Exh where

import Conduit
import Control.Concurrent
  ( MVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Error
import Control.Effect.Reader
import Control.Monad
import Control.Monad.Trans.Cont
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.HTTP.Client hiding (Cookie)
import Network.HTTP.Client.MultipartFormData

data Http :: Effect where
  FormRequest :: String -> Http m Request
  GenBoundary :: Http m ByteString
  RespOpen :: Request -> Http m (Response BodyReader)
  RespClose :: Response a -> Http m ()

formRequest :: Effs '[Http, Error HttpException] m => String -> m Request
formRequest = send . FormRequest
{-# INLINE formRequest #-}

genBoundary :: Eff Http m => m ByteString
genBoundary = send GenBoundary
{-# INLINE genBoundary #-}

respOpen :: Effs '[Http, Error HttpException] m => Request -> m (Response BodyReader)
respOpen = send . RespOpen
{-# INLINE respOpen #-}

respClose :: Eff Http m => Response a -> m ()
respClose = send . RespClose
{-# INLINE respClose #-}

data HttpH

instance (Effs '[Embed IO, Reader Manager] m) => Handler HttpH Http m where
  effHandler (FormRequest s) = embed @IO $ parseRequest s
  effHandler GenBoundary = embed webkitBoundary
  effHandler (RespOpen req) = ask >>= embed . responseOpen req
  effHandler (RespClose resp) = embed $ responseClose resp
  {-# INLINEABLE effHandler #-}

type HttpToIOC = InterpretC HttpH Http

httpToIO :: (Effs '[Embed IO, Reader Manager] m, Threaders '[ReaderThreads] m p) => HttpToIOC m a -> m a
httpToIO = interpretViaHandler
{-# INLINE httpToIO #-}

--------------------------------------------------
--

data Cookie :: Effect where
  TakeCookie :: Cookie m CookieJar
  ReadCookie :: Cookie m CookieJar
  PutCookie :: CookieJar -> Cookie m ()

takeCookie :: Eff Cookie m => m CookieJar
takeCookie = send TakeCookie
{-# INLINE takeCookie #-}

readCookie :: Eff Cookie m => m CookieJar
readCookie = send ReadCookie
{-# INLINE readCookie #-}

putCookie :: Eff Cookie m => CookieJar -> m ()
putCookie = send . PutCookie
{-# INLINE putCookie #-}

data CookieH

instance Effs '[Reader (MVar CookieJar), Embed IO] m => Handler CookieH Cookie m where
  effHandler TakeCookie = ask >>= embed . takeMVar
  effHandler ReadCookie = ask >>= embed . readMVar
  effHandler (PutCookie c) = do
    ref <- ask
    embed $ putMVar ref c
  {-# INLINEABLE effHandler #-}

type CookieToReaderC = InterpretC CookieH Cookie

type CookieToIOC = CompositionC '[CookieToReaderC, ReaderC (MVar CookieJar)]

cookieToIO :: (Eff (Embed IO) m, Threaders '[ReaderThreads] m p) => CookieToIOC m a -> m a
cookieToIO m = do
  ref <- embed $ newMVar mempty
  runReader ref $
    interpretViaHandler $
      runComposition m
{-# INLINE cookieToIO #-}

--------------------------------------------------
--

data ConduitIO :: Effect where
  RunConduitIO :: ConduitT () Void IO a -> ConduitIO m a

runConduitIO :: Eff ConduitIO m => ConduitT () Void IO a -> m a
runConduitIO = send . RunConduitIO
{-# INLINE runConduitIO #-}

data ConduitIOH

instance Eff (Embed IO) m => Handler ConduitIOH ConduitIO m where
  effHandler (RunConduitIO c) = embed $ runConduit c
  {-# INLINEABLE effHandler #-}

type ConduitIOToIOC = InterpretC ConduitIOH ConduitIO

conduitIOToIO :: Eff (Embed IO) m => ConduitIOToIOC m a -> m a
conduitIOToIO = interpretViaHandler
{-# INLINE conduitIOToIO #-}

--------------------------------------------------
--

attachFormData :: Eff Http m => [PartM m] -> Request -> m Request
attachFormData parts req = do
  boundary <- genBoundary
  formDataBodyWithBoundary boundary parts req

bodyReaderSource :: BodyReader -> ConduitT i ByteString IO ()
bodyReaderSource br = loop
  where
    loop = do
      bs <- lift br
      unless (B.null bs) $ do
        yield bs
        loop

resetCookie :: Eff Cookie m => m ()
resetCookie = takeCookie >> putCookie mempty
{-# INLINEABLE resetCookie #-}

modifyJar ::
  Effs '[Http, Cookie, Error HttpException, Bracket] m =>
  Request ->
  m ()
modifyJar req =
  bracketOnError
    takeCookie
    putCookie
    $ \jar -> do
      let req' = req {cookieJar = Just jar}
      bracket
        (respOpen req')
        respClose
        (putCookie . responseCookieJar)
{-# INLINEABLE modifyJar #-}

openWithJar ::
  Effs '[Http, Cookie, Error HttpException] m => Request -> m (Response (ConduitT i ByteString IO ()))
openWithJar req = do
  jar <- readCookie
  resp <- respOpen (req {cookieJar = Just jar})
  pure $ fmap bodyReaderSource resp

withSource ::
  Effs '[Http, Cookie, Error HttpException, Bracket] m =>
  Request ->
  ContT r m (Response (ConduitT i ByteString IO ()))
withSource req = ContT $ \k -> do
  jar <- readCookie
  bracket
    (respOpen (req {cookieJar = Just jar}))
    respClose
    (k . fmap bodyReaderSource)
{-# INLINEABLE withSource #-}
