module Util where

import ClassyPrelude
import Prelude (String)

import Data.Time
import System.Locale
import Data.Char (toLower, toUpper)

formatAwsTime :: UTCTime -> Text
formatAwsTime = pack . formatTime defaultTimeLocale "%Y-%m-%dT%TZ"

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []
