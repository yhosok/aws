{-# LANGUAGE OverloadedStrings #-}
module Config where

import Network.HTTP (RequestMethod (..))
import Data.Object.Yaml
import Data.Object
import Data.ByteString.Char8 (ByteString,pack)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (join)
import Network.HTTP.Types (QueryText)

data AWSConfig = AWSConfig { secKey :: ByteString
                           , accKey :: ByteString
                           , httpMethod :: ByteString
                           , host :: ByteString
                           , path :: ByteString
                           , commonParam :: QueryText
                           } deriving (Show)

mkCommonP :: ByteString -> QueryText
mkCommonP accKey = [("Version",Just "2011-12-15"),
                    ("SignatureVersion",Just "2"),
                    ("SignatureMethod",Just "HmacSHA256"),
                    ("AWSAccessKeyId",Just $ decodeUtf8 accKey)]

loadConf :: IO AWSConfig
loadConf = do
  obj <- join $ decodeFile "config.yml"
  confs <- fromMapping obj
  sec <- lookupScalar ("secKey"::String) confs
  acc <- lookupScalar "accKey" confs
  h <-  lookupScalar "host" confs
  p <- lookupScalar "path" confs
  return $ AWSConfig
    { secKey = sec
    , accKey = acc
    , host = h
    , httpMethod = pack . show $ POST
    , path = p
    , commonParam = mkCommonP acc
    }
