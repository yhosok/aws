{-# LANGUAGE OverloadedStrings #-}
module Config where

import ClassyPrelude

import Data.Yaml
import Data.Text.Encoding (decodeUtf8)
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
  mobj <- decodeFile "config.yml"
  case mobj of
    Just (Object obj) -> parseMonad parseConfig obj
    _ -> fail $ "can not find file : config.yml"

parseConfig :: Object -> Parser AWSConfig
parseConfig o = do
  sec <- o .: "secKey"
  acc <- o .: "accKey"
  h <- o .: "host"
  p <- o .: "path"
  return $ AWSConfig
    { secKey = sec
    , accKey = acc
    , host = h
    , httpMethod = "POST"
    , path = p
    , commonParam = mkCommonP acc
    }

