{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Request.DescribeSpotInstanceRequests where

import ClassyPrelude

import Data.Text (Text,pack)
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Default (Default (def))

import Request
import Request.RequestTH
import Request.RequestDefParser
import Request.Filter

$(mkDefsD [request|
DescribeSpotInstanceRequests
  SpotInstanceRequestId List String No
  Filter_ List Data Filter No
|])

