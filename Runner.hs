module Msl.Runner where

import Text.ParserCombinators.Parsec
import Msl.AST
import Msl.Lexer
import Msl.Parser

fromMsl :: String -> Either ParseError [Decl]
fromMsl s = 
  case parse mslLexer "fromMsl" s of
    Left e -> Left e
    Right ls -> parse mslProgram "fromMsl" ls

printLisp :: String -> IO ()
printLisp text = printLisp' 0 (lines text)
  where
    printLisp' _ [] = return ()
    printLisp' i (l:ls) = do
      putStr $ indent i
      putStrLn l
      printLisp' (i + balance l) ls

    balance s = length (filter (=='(') s) - length (filter (==')') s)

    indent i = (i*2) `replicate` ' '

processFile fn = do
  text <- readFile fn
  case fromMsl text of
    Left e -> print $ show e
    Right asts -> mapM_ (printLisp . show) asts
