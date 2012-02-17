{-# LANGUAGE OverloadedStrings #-}
module Filters 
       (
         module Filters
       )
       where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

extract :: Filter a -> L.ByteString -> a
extract f = f . fromDocument . doc def
  where doc = parseLBS_

type Filter a = Cursor -> a

findContent :: Text -> Filter Text
findContent name = fmap head (findContents name)

findContents :: Text -> Filter [Text]
findContents name =  child &// laxElement name &/ content

instanceId :: Filter Text
instanceId =  findContent "instanceId"

spotInstanceRequestId, state :: Filter Text
spotInstanceRequestId = findContent "spotInstanceRequestId"
state = findContent "state" 
