{-# LANGUAGE DeriveDataTypeable #-}
module Request.RequestDefParser where

import ClassyPrelude hiding (readFile)

import Prelude (String, readFile)

import Text.Parsec
import Text.Parsec.String

import Data.Generics

data RequestDef = RPD { dataName :: String
                      , fields :: [FieldDef]
                      } deriving (Show, Typeable, Data)

data FieldDef = FD { recName :: String
                   , recType :: RecType
                   , require :: Bool
                   } deriving (Show, Typeable, Data)
                
data RecType = Text | Int | Bool | UTCTime | List RecType | Data String
             deriving (Eq,Show,Typeable,Data)


parseReqFile :: String -> IO (Either ParseError [RequestDef])
parseReqFile fn = do
  input <- readFile fn
  return $ parse parser fn input

parseReq :: String -> [RequestDef]
parseReq = ret . parse parser ""
  where ret (Left e) = error $ show e
        ret (Right r) = r

parser :: Parser [RequestDef]
parser = do
  many newline
  rs <- many reqdef_p
  eof
  return rs

reqdef_p :: Parser RequestDef
reqdef_p = do
  h <- header
  newline
  fs <- many1 fddef_p 
  return $ RPD h fs

header :: Parser String
header = do
  h <- upper
  hs <- many alphaNum
  many $ char ' '
  return (h:hs)

fddef_p :: Parser FieldDef
fddef_p = do
  many1 space
--  n <- lower
  fn <- fdname_p
  many1 space
  ty <- type_p
  many1 space
  req <- req_p
  many $ char ' '  
  many1 newline
  return $ FD fn ty req

fdname_p :: Parser String
fdname_p = many fdchar
  where fdchar = alphaNum <|> char '_'

type_p :: Parser RecType
type_p = try ptype <|> try ltype <|> dtype

ptype :: Parser RecType
ptype = (string "String" >> return Text) 
    <|> (string "Integer" >> return Int)
    <|> (string "Boolean" >> return Bool)
    <|> (string "DateTime" >> return UTCTime)

ltype :: Parser RecType
ltype = do
  string "List"
  many1 space
  pt <- try ptype <|> dtype
  return $ List pt

dtype :: Parser RecType
dtype = do
  string "Data"
  many space
  s <- many alphaNum
  return $ Data s

req_p :: Parser Bool
req_p = (string "Yes" >> return True)
    <|> (string "No" >> return False)