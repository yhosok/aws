{-# LANGUAGE OverloadedStrings #-}
module Filters 
       (
         module Filters
       )
       where

import ClassyPrelude

import Prelude (head)

import Text.XML
import Text.XML.Cursor

extract :: Filter a -> LByteString -> a
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
