{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Request.Filter where

import ClassyPrelude

import Data.Text (Text)
import Control.Applicative
import Data.Default (Default (def))

import Request.RequestTH
import Request.RequestDefParser
import Request

$(mkDefsD [request|
Filter
  Name String Yes
  Value List String Yes
|])
