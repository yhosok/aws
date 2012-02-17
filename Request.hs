{-# LANGUAGE TypeSynonymInstances, OverloadedStrings #-}
module Request where

import qualified Data.Text as T
import Control.Arrow (first)
import Data.Time
import Network.HTTP.Types (QueryText)
import Control.Arrow(second)

import Util

class (ToParam a) => ActionParam a where
  actionName :: a -> T.Text
  actionParam :: a -> QueryText
  actionParam a = ("Action", Just $ actionName a) : toParam a

class ToParam a where
  toParam :: a -> QueryText

instance ToParam T.Text where
  toParam bs = [("",Just bs)]
  
instance ToParam Int where
  toParam i = [("", Just $ toT i)]

instance ToParam UTCTime where
  toParam t = [("", Just $ formatAwsTime t)]

instance (ToParam a) => ToParam (Maybe a) where
  toParam (Just v) = toParam v
  toParam Nothing = []

instance (ToParam a) => ToParam [a] where
  toParam vs = concat $ zipWith mk  [1..] vs
    where mk idx = toParamWithPrefix (toT idx)

toT :: (Show a) => a -> T.Text
toT = T.pack . show

toShowParam :: (Show a) => a -> QueryText
toShowParam = (:[]) . second Just . T.break (==' ') . T.pack . show

addPrefix :: T.Text -> (T.Text, a) -> (T.Text, a)
addPrefix = first . ap
  where ap pfx "" = pfx
        ap pfx x = T.concat [pfx ,".",x]

toParamWithPrefix :: (ToParam a) => T.Text -> a -> QueryText
toParamWithPrefix pfx = fmap (addPrefix pfx) . toParam
