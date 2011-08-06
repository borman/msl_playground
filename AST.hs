module Msl.AST where

import Data.List

data Decl = Global String
          | Fun String Expr [Oper]

data Oper = If Expr [Oper] [Oper]
          | For String Expr Expr [Oper]
          | While Expr [Oper]
          | Let Expr Expr
          | Return Expr
          | Do Expr  

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

data Infix = Equals | Less | Greater
           | Plus | Minus 
           | Mul | Div | Mod
           | And | Or

instance Show Decl where
  show (Global name) = "(global " ++ name ++ ")"
  show (Fun name expr block) = "(fun (" ++ name ++ " " ++ show expr ++ ")\n" ++ show block ++ ")"

instance Show Oper where
  show (If c t []) = "(if " ++ show c ++ "\n" ++ show t ++ ")"
  show (If c t e) = "(if " ++ show c ++ "\n" ++ show t ++ "\n" ++ show e ++ ")"
  show (For v f t b) = "(for " ++ show v ++ " (from " ++ show f ++ ") (to " ++ show t ++ ")\n" ++ show b ++ ")"
  show (While c b) = "(while " ++ show c ++ "\n" ++ show b ++ ")"
  show (Let l r) = "(let " ++ show l ++ " " ++ show r ++ ")"
  show (Return e) = "(return " ++ show e ++ ")"
  show (Do e) = "(do " ++ show e ++ ")"

  showList ops s =  (intercalate "\n" $ map show ops) ++ s

instance Show Expr where
  show (Integer i) = show i
  show (Boolean b) = show b
  show (Real r) = show r
  show (String s) = show s
  show (Var v) = v
  show (ArrayItem n i) = "(idx " ++ n ++ " " ++ show i ++ ")"
  show (FuncCall f x) = "(call " ++ f ++ " " ++ show x ++ ")"
  show (Tuple es) = "(" ++ (intercalate " " $ map show es) ++ ")"
  show (Cond c t e) = "(if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Infix i l r) = "(" ++ show i ++ " " ++ show l ++ " " ++ show r ++ ")"

instance Show Infix where
  show Equals = "=="
  show Less = "<"
  show Greater = ">"
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "mod"
  show And = "and"
  show Or = "or"
