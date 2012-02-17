{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module AWS 
       (
         run,
         runWithFilter
       )
       where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Base64 (encode)
import qualified Blaze.ByteString.Builder as Builder
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Digest.Pure.SHA
import Network.HTTP.Conduit
import Network.HTTP.Types (renderQuery,queryTextToQuery,Query,QueryText,
                           status200,status300)
import Data.List
import Data.Time
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Function (on)
import System.Locale
import Control.Failure
import Control.Arrow(second)

import qualified Config
import Filters

--import DebugUtil

createSign :: Config.AWSConfig -> QueryText -> S.ByteString
createSign conf = encode . calc . signStr . renderQuery False . queryTextToQuery . sortParam
  where signStr q = S.intercalate "\n" [ Config.httpMethod conf
                                       , Config.host conf
                                       , Config.path conf
                                       , q
                                       ]
        sortParam = sortBy (compare `on` fst)
        calc = toS . bytestringDigest . hmacSha256 (toL $ Config.secKey conf) . toL
        toS = Builder.toByteString . Builder.fromLazyByteString
        toL = Builder.toLazyByteString . Builder.fromByteString

reqUrl :: Config.AWSConfig -> QueryText -> S.ByteString
reqUrl conf p = "https://" `S.append` 
                Config.host conf `S.append` 
                Config.path conf `S.append` 
                "?" `S.append` 
                renderQuery False (queryTextToQuery p)

queryParam :: String -> QueryText -> Config.AWSConfig -> Query
queryParam ts p conf = ("Signature",Just $ createSign conf p') : queryTextToQuery p'
  where p' = ("Timestamp",Just $ T.pack ts) : p ++ Config.commonParam conf

run :: (MonadIO m, Failure HttpException m) =>
             QueryText -> m L.ByteString
run p = do
  t <- liftIO timestamp
  conf <- liftIO Config.loadConf
  let p' = queryParam t p conf
--  simpleHttp (S8.unpack $ reqUrl p')
  Response sc h b <- liftIO $ withManager $ httpLbs (request p' conf)
  if status200 <= sc && sc < status300
    then return b
    else failure $ StatusCodeException sc h
  where 
    timestamp = do utc_time <- getCurrentTime
                   return $ formatTime defaultTimeLocale "%Y-%m-%dT%TZ" utc_time
    request p conf = def { host = Config.host conf
                         , port = 443
                         , secure = True
                         , requestHeaders = headers
                         , path = Config.path conf
                         , queryString = renderQuery False p
                         , method = Config.httpMethod conf
                         }
    headers = [ ("ContentType", "application/x-www-form-urlencoded; charset=utf-8") ]

runWithFilter :: (MonadIO m, Failure HttpException m) =>
                 QueryText -> Filter a -> m a
runWithFilter p filter = do
  xml <- run p
  return $ extract filter xml