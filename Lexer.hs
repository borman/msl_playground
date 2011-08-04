module Msl.Lexer (Lexem(..), mslLexer) where

import Text.ParserCombinators.Parsec
import Msl.AST

data Lexem = Symbol String
           | Value Expr
           | NameV String
           | NameF String
           | NameA String
     deriving (Show)

mslLexer = whitespace >> mytoken `endBy` whitespace

mytoken = choice' [
    wrap Symbol symbol
  , wrap (Value . String) literal
  , wrap (Value . Boolean) bool
  , wrap (Value . Real ) real
  , wrap (Value . Integer) integer
  , wrap NameV nameV
  , wrap NameA nameA
  , wrap NameF nameF]

whitespace = skipMany ((space >> return ()) <|> comment)
comment = do
  char ';'
  manyTill anyChar newline
  return ()

-- Keywords

syms = [
  "fun", "global", "end", "return",
  "for", "from", "to", "while", "do",
  "if", "then", "else", 
  "=", "<", ">", "and", "or",
  "+", "-", "*", "/", "%",
  ",", "(", ")", "[", "]"
  ]

symbol = choice' $ map string syms

-- Constants

bool :: Parser Bool
bool = (string "true" >> return True) <|> (string "false" >> return False)

literal :: Parser String
literal = between (char '"') (char '"') $ many escaped
  where
    escaped = noneOf "\\\"" <|> escape
    escape = do
      char '\\'
      e <- oneOf "\"n"
      return $ case e of
        '\"' -> '\"'
        'n' -> '\n'

signMark :: Num n => Parser (n -> n)
signMark = option (\x -> x) neg
  where
    neg = do
      char '~'
      return (\x -> -x)
 
integer :: Parser Integer
integer = do
  sign <- signMark
  digits <- many1 digit
  return (sign $ read digits)

real :: Parser Double
real = do
  sign <- signMark
  int <- option "0" (many1 digit)
  char '.'
  frac <- many1 digit
  return (sign $ read (int ++ "." ++ frac))


-- Identifiers

nameLetter = alphaNum <|> char '_'

nameV :: Parser String
nameV = do
  first <- upper
  remaining <- many nameLetter
  return ([first] ++ remaining)

nameF :: Parser String
nameF = do
  first <- lower
  remaining <- many nameLetter
  return ([first] ++ remaining)

nameA :: Parser String
nameA = do
  char '$'
  nameV

-- Utils

wrap f p = do
  pos <- getPosition
  v <- p
  return $ (pos, f v)

choice' cs = choice $ map try cs
