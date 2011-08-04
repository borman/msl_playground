module Msl.AST where

data Decl = Global String
          | Fun String Expr [Oper]
     deriving (Show)

data Oper = If Expr [Oper] [Oper]
          | For String Expr Expr [Oper]
          | While Expr [Oper]
          | Let Expr Expr
          | Return Expr
          | Do Expr  
     deriving (Show)

data Expr = Integer Integer
          | Boolean Bool
          | Real Double
          | String String
          | Var String
          | ArrayItem String Expr
          | FuncCall String Expr
          | Tuple [Expr]
          | Cond Expr Expr Expr
          | Infix Infix Expr Expr
     deriving (Show)

data Infix = Equals | Less | Greater
           | Plus | Minus 
           | Mul | Div | Mod
           | And | Or
     deriving (Show)
