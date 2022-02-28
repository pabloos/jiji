
module Parser.Program where

import Control.Applicative

import Parser.Combinators
import Parser.Tokens
import Parser.AST
import Parser.Expr
import Parser.Statement
import Parser.Definition

-- run :: String -> Program
-- run = runParser program

program :: Parser Program
program = Program <$> many definition <*> main

main :: Parser Main
main = Main <$> statement
