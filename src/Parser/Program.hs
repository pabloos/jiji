
module Parser.Program where

import Control.Applicative

import Parser.Combinators
import Parser.Tokens
import Parser.AST
import Parser.Expr
import Parser.Statement
import Parser.Definition

program :: Parser Program
program = Program <$> many definition <*> main

main :: Parser Main
main = do
    reserved "fun"
    reserved "main"
    reserved "("
    reserved ")"
    reserved "{"
    statements <- many statement
    reserved "}"
    return $ Main statements