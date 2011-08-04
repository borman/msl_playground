module Msl.Parser where

import Text.ParserCombinators.Parsec
import Msl.AST
import Msl.Lexer

type MslParser a = GenParser (SourcePos, Lexem) () a

program :: MslParser [Decl]
program = do
  decls <- many decl
  eof
  return decls
  
decl :: MslParser Decl
decl = choice [globalVar, fun]

globalVar = do
  sym "global"
  name <- nameV
  return $ Global name

fun = do
  sym "fun"
  name <- nameF
  args <- lExpr
  ops <- block
  return $ Fun name args ops

-- Operators

block = do
  ops <- many operator
  sym "end"
  return ops

operator = choice [ifOp, forOp, whileOp, returnOp, try letOp, doOp] <?> "operator"

ifOp = do
  sym "if"
  c <- expr
  sym "then"
  t <- block
  e <- option [] $ try (sym "else" >> block)
  return $ If c t e

forOp = do
  sym "for"
  v <- nameV
  sym "from"
  f <- expr
  sym "to"
  t <- expr
  sym "do"
  d <- block
  return $ For v f t d

whileOp = do
  sym "while"
  c <- expr
  sym "do"
  d <- block
  return $ While c d

returnOp = do
  sym "return"
  e <- expr
  return $ Return e

letOp = do
  l <- lExpr
  sym "="
  e <- expr
  return $ Let l e

doOp = do
  e <- simpleExpr
  return $ Do e

-- Infix expressions

expr = stmtE `chainl1` logOp
stmtE = sumE `chainl1` cmpOp
sumE = prodE `chainl1` addOp
prodE = simpleExpr `chainl1` mulOp

logOp = op "and" And <|> op "or" Or
cmpOp = op "<" Less  <|> op ">" Greater <|> op "=" Equals
addOp = op "+" Plus  <|> op "-" Minus
mulOp = op "*" Mul   <|> op "/" Div     <|> op "%" Mod

op s t = sym s >> return (Infix t)

-- Simple expressions

simpleExpr :: MslParser Expr
simpleExpr = choice' [
  val
  , wrap Tuple (tuple expr)
  , paren
  , cond
  , wrap Var nameV
  , wrap2 ArrayItem nameA simpleExpr
  , wrap2 FuncCall  nameF simpleExpr]

lExpr = choice' [
  wrap Tuple (tuple lExpr)
  , wrap Var nameV
  , wrap2 ArrayItem nameA simpleExpr]

tuple e = braced "[" "]" $ e `sepBy` sym ","
paren = braced "(" ")" expr

cond = do
  sym "if"
  c <- expr
  sym "then"
  t <- expr
  sym "else"
  e <- expr
  return $ Cond c t e

-- Utils

braced l r = between (sym l) (sym r)

wrap f p = do
  v <- p
  return $ f v

wrap2 f p1 p2 = do
  v1 <- p1
  v2 <- p2
  return $ f v1 v2

choice' cs = choice $ map try cs

-- Parser basis

val = mytoken (\t -> case t of
                  Value v -> Just v
                  otherwise -> Nothing)

nameV = mytoken (\t -> case t of
                    NameV n -> Just n
                    otherwise -> Nothing)
nameA = mytoken (\t -> case t of
                    NameA n -> Just n
                    otherwise -> Nothing)
nameF = mytoken (\t -> case t of
                    NameF n -> Just n
                    otherwise -> Nothing)

sym s = mytoken (\t -> case t of
                    Symbol s' | s==s' -> Just s
                    otherwise -> Nothing)

mytoken :: (Lexem -> Maybe a) -> MslParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok




