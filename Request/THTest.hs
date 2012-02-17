{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Request.THTest where

import Request.RequestTH
import Request.RequestDefParser
import Request

import Data.Default (Default (def))
import Data.Text
import Control.Applicative
import Data.Maybe (catMaybes)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Ppr

import Test.QuickCheck
import Test.QuickCheck.All


$(mkDefsD [request|
Test
    field1 String Yes
    field2 List String Yes
    field3 Data Test2 Yes
Test2
    field21 String No
    field22 List Integer Yes
    field23 Integer No
|])

-- $(defsFromFile "models")
t = Test {field1 = "abc", field2 = ["ddd","eee"], field3= t3}
t2 = Test2 {field21 = Just "test2", field22 = [3,4,5], field23 = Just 8}
t3 = Test2 {field21 = Just "test3", field22 = [6,7,8], field23 = Just 10}

instance Arbitrary Text where
  arbitrary = fmap pack (arbitrary :: Gen String)

instance Arbitrary Test2 where
  arbitrary = Test2
           <$> arbitrary
           <*> arbitrary
           <*> arbitrary

prop_test f1 f2 f3 = toParam t == p
  where t = Test {field1 = f1,field2 = f2, field3 = f3}
        p = [("Field1",Just f1)] ++ (toParamWithPrefix "Field2" f2) ++ (toParamWithPrefix "Field3" f3)

x = Test "" [] (Test2 (Just "") [] (Just 1))

runtest = $quickCheckAll

--x' = [("Field1","")] ++ (toParamWithPrefix "Field2" ([]::[ByteString])) ++ (toParam t2)
--  where t2 = Test2 (Just "") [] (Just 1)