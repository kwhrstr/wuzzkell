{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http where

import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Client as L
import qualified Text.URI as URI
import qualified Data.CaseInsensitive as CI
import qualified RIO.ByteString as BS
import System.IO
import Control.Lens.TH

data HttpRequest = HttpRequest
  { _reqRawUri :: Text
  , _reqMethod :: Http.StdMethod
  , _reqRawParams :: Text
  , _reqRawHeaders :: Text
  , _reqRawBody :: Text
  }deriving Show
makeLenses ''HttpRequest

data HttpResponse = HttpResponse
  { _responseHeader :: Text
  , _responseBody :: Text
  }
makeLenses ''HttpResponse

newtype ParseError = ParseError Text deriving Show
instance Exception ParseError


parseHttpMethod :: MonadThrow m => Text -> m Http.StdMethod
parseHttpMethod "" = pure Http.GET
parseHttpMethod t = case readMaybe . T.unpack $ t of
  Nothing -> throwM $ ParseError $ "undefined method" <> t
  Just method -> pure method



parseURL :: MonadThrow m
         => Text
         -> m (Either (Req.Url 'Req.Http, Req.Option s0) (Req.Url 'Req.Https, Req.Option s1))
parseURL rawUri = do
  uri <- URI.mkURI rawUri
  case Req.useURI uri of
    Nothing -> throwM $ ParseError $ "url parse error: " <> rawUri
    Just eitherHttpHttps -> pure eitherHttpHttps

parseHeaders :: Text -> Req.Option sc
parseHeaders t =
  let breakOnWithDrop drp txt = T.dropPrefix drp <$> T.breakOn drp txt
      ts = map (breakOnWithDrop ": ") $ T.linesCR t
      both f (a, b) = (f a, f b)
  in foldl' (\acc a -> acc <> (uncurry Req.header . both (T.encodeUtf8 . T.strip) $ a)) mempty  ts

doRequest :: HttpRequest -> IO HttpResponse
doRequest r = do
  res <- try $ Req.runReq Req.defaultHttpConfig $ reqCli r
  case res of
    Left (e :: SomeException) -> pure $ 
      HttpResponse (T.pack $ displayException e) ""
    Right st' -> pure st'
  

decodeUtf8WithThrow :: MonadThrow m => ByteString -> m Text
decodeUtf8WithThrow = either throwM pure . decodeUtf8'

reqCli :: (Req.MonadHttp m)=> HttpRequest -> m HttpResponse
reqCli r = do
  httpOrHttps <- liftIO $ parseURL $ r ^. reqRawUri
  let queryParams = concatMap (Http.parseQueryText . T.encodeUtf8) $ T.lines $ r ^. reqRawParams
      headers = parseHeaders $ r ^. reqRawHeaders
      body = Req.ReqBodyBs $ T.encodeUtf8 $ r ^. reqRawBody
      stMethod = r ^. reqMethod
  response <- case httpOrHttps of
    Left (http, options) -> req stMethod http body Req.bsResponse
        $  options <> headers
        <> foldl' (\acc a -> acc <> uncurry  Req.queryParam a) mempty queryParams
    Right (https, options) -> req stMethod https body Req.bsResponse
        $  options <> headers
        <> foldl' (\acc a -> acc <> uncurry  Req.queryParam a) mempty queryParams
  resBodyTxt <- liftIO $ decodeUtf8WithThrow $ Req.responseBody response
  resHeaderTxt <- liftIO $ decodeUtf8WithThrow $ BS.intercalate "\n" $ map (\(a, b) -> CI.original a <> ": " <> b) $ responseHeaders response
  
  let res = HttpResponse resHeaderTxt resBodyTxt
  pure res
  where
    req Http.GET method _     = Req.req Req.GET method Req.NoReqBody
    req Http.POST method b    = Req.req Req.POST method b
    req Http.PUT method b     = Req.req Req.PUT method b
    req Http.PATCH method b   = Req.req Req.PATCH method b
    req Http.HEAD method _    = Req.req Req.HEAD method Req.NoReqBody
    req Http.OPTIONS method _ = Req.req Req.OPTIONS method Req.NoReqBody
    req Http.DELETE method _  = Req.req Req.DELETE method Req.NoReqBody
    req Http.TRACE method b   = Req.req Req.TRACE method b
    req Http.CONNECT method b = Req.req Req.CONNECT method b
  
responseHeaders :: Req.HttpResponse response
                => response          -- ^ Response interpretation
                -> [Http.Header]
responseHeaders = L.responseHeaders . Req.toVanillaResponse


initialHttpReq :: HttpRequest
initialHttpReq = HttpRequest
  { _reqRawUri = "https://"
  , _reqRawParams = ""
  , _reqMethod = Http.GET
  , _reqRawBody = ""
  , _reqRawHeaders = ""
  }





  