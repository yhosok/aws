{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Request.DescribeSnapshots where

import Data.Text (Text)
import Data.Default (Default (def))

import Request
import Request.RequestTH
import Request.RequestDefParser
import Request.Filter

$(mkDefsD [request|
DescribeSnapshots
  SnapshotId List String No
  Owner List String No
  RestorableBy List String No
  Filter_ List Data Filter No
|])
