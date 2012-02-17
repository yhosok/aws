{-# LANGUAGE TemplateHaskell #-}
module Request.RequestTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Ppr

import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Default (Default (def))
import qualified Data.Text as T(Text,pack)
import Network.HTTP.Types (QueryText)

import Request
import Request.RequestDefParser
import Util

request :: QuasiQuoter
request = QuasiQuoter { quoteExp = dataToExpQ (const Nothing) . parseReq,
                        quotePat = undefined,
                        quoteType = undefined,
                        quoteDec = mkDefsD . parseReq }

defsFromFile :: String -> Q [Dec]
defsFromFile fn = do
  rs <- runIO $ parseReqFile fn
  case rs of
    Left e -> fail $ show e
    Right rs -> mkDefsD rs

mkDefsD :: [RequestDef] -> Q [Dec]
mkDefsD = fmap concat . mapM mkDefD 

mkDefD :: RequestDef -> Q [Dec]
mkDefD rd = do
  d <- mkDataD rd
  p <- mkToParamD rd
  a <- mkActionParamD rd
  def <- mkDefaultD rd
  return [d,p,a,def]

mkDataD :: RequestDef -> Q Dec
mkDataD rd =  do
  dataD (cxt []) name [] [recC name cols] [mkName "Show",mkName "Eq"]
  where name = mkName $ dataName rd
        cols = map mkRec $ fields rd
        mkRec fd = do
          let n = mkName $ lowerFirst $ recName fd
          t <- ty fd
          return (n, NotStrict,t)
        ty fd | require fd = [t| $(tp $ recType fd) |]
              | otherwise = [t| Maybe $(tp $ recType fd) |]
        tp (List a) = [t| [$(tp a)] |]
        tp (Data x) = conT $ mkName x
        tp a = conT $ mkName . show $ a

mkToParamD :: RequestDef -> Q Dec
mkToParamD rd = instanceD (return []) cls [fun] 
  where cls = conT (mkName "ToParam") `appT` conT dName
        fun = funD funName [clause [recP dName recs] exp []]
        recs = fmap rec $ fields rd
        rec fd = fieldPat (mkName $ fieldName fd) (varP $ mkName (fieldName fd))
        exp  = normalB $ exp' $ fields rd
        exp' [] = [| [] |]
        exp' (f:fs) = [| $(mkExp f) ++ $(exp' fs)|] 
        dName = mkName $ dataName rd
        funName = mkName "toParam"

mkExp :: FieldDef -> ExpQ
mkExp fd | isList t   = [| toParamWithPrefix (T.pack n) $v |]
         | require fd = [| [(T.pack n, Just $ $val $v)] |]
         | otherwise  = [| [(T.pack n, fmap $val $v)] |]
--         | require fd = [| [$(mkExp' fd) $v] |]
--         | otherwise  = [| catMaybes [$([| fmap $(mkExp' fd) $v |])] |]
  where val | t == Bool || t == Int = [| toT |]
            | otherwise = [| id |]
        v = varE $ mkName $ fieldName fd
        n = paramName fd
        t = recType fd
        isList (List _) = True
        isList (Data _) = True
        isList UTCTime = True
        isList _ = False

{--
mkExp' fd | t == Text = [| \v -> (T.pack n, v) |]
          | t == Int = [| \v -> (T.pack n, toT v) |]
          | t == Bool = [| \v -> (T.pack n, toT v) |]
          | otherwise = [| \v -> toParam v |]
  where t = recType fd
        n = paramName fd
--} 

fieldName :: FieldDef -> String
fieldName = lowerFirst . recName

paramName :: FieldDef -> String
paramName = upperFirst . filter (/= '_') . recName

mkActionParamD :: RequestDef -> Q Dec
mkActionParamD rd = instanceD (return []) cls [fun]
  where cls = conT (mkName "ActionParam") `appT` conT dName
        fun = funD funName [clause [wildP] exp []]
        exp = normalB $ [| T.pack n |]
        dName = mkName $ dataName rd
        n = dataName rd
        funName = mkName "actionName"

mkDefaultD :: RequestDef -> Q Dec
mkDefaultD rd =  instanceD (return []) cls [fun]
  where cls = conT (mkName "Default") `appT` conT dName
        fun = funD funName [clause [] exp []]
        exp = normalB $ recConE dName (fmap mkFieldDef $ fields rd)
        dName = mkName $ dataName rd
        n = dataName rd
        funName = mkName "def"

mkFieldDef :: FieldDef -> Q (Name, Exp)
mkFieldDef fd = fieldExp n (defval t)
  where n = mkName $ fieldName fd
        t = recType fd
        defval _ | not $ require fd = [| Nothing |]
        defval Text = [| "" |]
        defval Int = [| 0 |]
        defval Bool = [| False |]
        defval (List _) = [| [] |]
        defval (Data a) = [| def |]