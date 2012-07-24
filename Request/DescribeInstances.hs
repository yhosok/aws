{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Request.DescribeInstances where

import ClassyPrelude

import Data.Text (Text)
import Control.Applicative
import Data.Default (Default (def))

import Request
import Request.RequestTH
import Request.RequestDefParser
import Request.Filter

$(mkDefsD [request|
DescribeInstances
  InstanceId List String No
  Filter_ List Data Filter No
|])
