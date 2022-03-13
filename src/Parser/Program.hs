
module Parser.Program where

import Control.Applicative

import Parser.Combinators
import Parser.Tokens
import Parser.AST
import Parser.Expr
import Parser.Statement
import Parser.Definition

program :: Parser AST
program = do -- Program <$> many definition <*> main
    many1 definition