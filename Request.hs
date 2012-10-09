{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Request where

import ClassyPrelude
import Data.Time
import Network.HTTP.Types (QueryText)
import Data.List (zipWith)

import Util

class (ToParam a) => ActionParam a where
  actionName :: a -> Text
  actionParam :: a -> QueryText
  actionParam a = ("Action", Just $ actionName a) : toParam a

class ToParam a where
  toParam :: a -> QueryText

instance ToParam Text where
  toParam bs = [("",Just bs)]
  
instance ToParam Int where
  toParam i = [("", Just $ toT i)]

instance ToParam UTCTime where
  toParam t = [("", Just $ formatAwsTime t)]

instance (ToParam a) => ToParam (Maybe a) where
  toParam (Just v) = toParam v
  toParam Nothing = []

instance (ToParam a) => ToParam [a] where
  toParam = concat . zipWith mk  [1..]
    where mk idx = toParamWithPrefix (toT idx)

toT :: (Show a) => a -> Text
toT = pack . show

toShowParam :: (Show a) => a -> QueryText
toShowParam = (:[]) . second Just . break (==' ') . pack . show

addPrefix :: Text -> (Text, a) -> (Text, a)
addPrefix = first . ap
  where ap pfx "" = pfx
        ap pfx x = concat [pfx ,".",x]

toParamWithPrefix :: (ToParam a) => Text -> a -> QueryText
toParamWithPrefix pfx = fmap (addPrefix pfx) . toParam
