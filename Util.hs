module Util where

import qualified Data.Text as T
import Data.Time
import System.Locale
import Data.Char

formatAwsTime :: UTCTime -> T.Text
formatAwsTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%TZ"

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []
