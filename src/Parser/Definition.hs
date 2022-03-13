module Parser.Definition where

import Control.Applicative

import Parser.Expr
import Parser.AST
import Parser.Combinators
import Parser.Statement
import Parser.Type

definition :: Parser Definition
definition = procedure <|> function <|> constant

constant :: Parser Definition
constant = do
    reserved "const"
    name <- symbol
    expr <- expr
    return $ Constant name expr

function :: Parser Definition
function = do 
    reserved "fun"
    name <- symbol
    args <- args
    type_ <- typeValue
    reserved "{"
    body <- many statement
    reserved "}"
    return $ Function name args type_ body

procedure :: Parser Definition
procedure = do
    reserved "fun"
    name <- symbol
    args <- args
    reserved "->"
    typeDef <- typeValue
    reserved "{"
    body <- many statement
    reserved "}"
    return $ Procedure name args body 