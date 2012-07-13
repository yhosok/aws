{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}
module AWS 
       (
         run
       , runWithFilter
       )
       where

import ClassyPrelude

import Data.ByteString (intercalate)
import Data.ByteString.Base64 (encode)
import qualified Blaze.ByteString.Builder as Builder
import Data.Digest.Pure.SHA
import Network.HTTP.Conduit
import Network.HTTP.Types (renderQuery,queryTextToQuery,Query,QueryText,
                           status200,status300)
import Data.List (sortBy)
import Data.Time
import Data.Function (on)
import Data.Typeable (Typeable)
import System.Locale
import Control.Failure
import Data.Maybe (isJust)

import qualified Config
import Filters

--import DebugUtil

createSign :: Config.AWSConfig -> QueryText -> ByteString
createSign conf = encode . calc . signStr . renderQuery' . queryTextToQuery . sortParam
  where signStr q = intercalate "\n" [ Config.httpMethod conf
                                       , Config.host conf
                                       , Config.path conf
                                       , q
                                       ]
        sortParam = sortBy (compare `on` fst)
        calc = toS . bytestringDigest . hmacSha256 (toL $ Config.secKey conf) . toL
        toS = Builder.toByteString . Builder.fromLazyByteString
        toL = Builder.toLazyByteString . Builder.fromByteString

reqUrl :: Config.AWSConfig -> Query -> ByteString
reqUrl conf p = "https://" `mappend` 
                Config.host conf `mappend` 
                Config.path conf `mappend` 
                "?" `mappend` 
                renderQuery' p

queryParam :: [Char] -> QueryText -> Config.AWSConfig -> Query
queryParam ts p conf = ("Signature",Just $ createSign conf p') : queryTextToQuery p'
  where p' = ("Timestamp",Just $ pack ts) : p ++ Config.commonParam conf

renderQuery' = renderQuery False . filter (isJust . snd)
--renderQuery' = renderQuery False

data AWSException = AWSException LByteString deriving (Show, Typeable)
instance Exception AWSException

run :: (MonadIO m, Failure AWSException m) =>
       QueryText -> m LByteString
run p = do
  t <- liftIO timestamp
  conf <- liftIO Config.loadConf
  let p' = queryParam t p conf
--  simpleHttp (S8.unpack $ reqUrl conf p')
  Response sc _ h b <-  liftIO $ withManager $ httpLbs (request p' conf)
  if status200 <= sc && sc < status300
    then return b
    else do
      failure $ AWSException b
  where 
    timestamp = do utc_time <- getCurrentTime
                   return $ formattm utc_time
    formattm = formatTime defaultTimeLocale "%Y-%m-%dT%TZ"
    request p conf = def { host = Config.host conf
                         , port = 443
                         , secure = True
                         , requestHeaders = headers
                         , path = Config.path conf
                         , queryString = renderQuery' p
                         , method = Config.httpMethod conf
                         , checkStatus =  \s hs -> Nothing
                         }
    headers = [ ("ContentType", "application/x-www-form-urlencoded; charset=utf-8") ]

runWithFilter :: (MonadIO m, Failure AWSException m) =>
                 QueryText -> Filter a -> m a
runWithFilter p filter = do
  xml <- run p
  return $ extract filter xml
