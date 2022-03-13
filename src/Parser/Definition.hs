module Parser.Definition where

import Control.Applicative

import Parser.Expr
import Parser.AST
import Parser.Combinators
import Parser.Statement
import Parser.Type
import Parser.Literal

definition :: Parser Definition
definition = procedure <|> function <|> constant

constant :: Parser Definition
constant = do
    reserved "const"
    name <- symbol
    reserved "="
    value <- basicValue
    return $ Constant name value

basicValue :: Parser Literal
basicValue = do
    IntVal <$> intLiteral <|> StringVal <$> stringLiteral <|> BoolVal <$> boolLiteral

function :: Parser Definition
function = do
    reserved "fun"
    name <- symbol
    params <- params
    reserved "->"
    returnType <- typeValue
    Subprogram name params (Just returnType) <$> stmtBlock

procedure :: Parser Definition
procedure = do
    reserved "fun"
    name <- symbol
    params <- params
    Subprogram name params Nothing <$> stmtBlock

params :: Parser [Param]
params = do
    parens $ sepBy param (reserved ",")

param :: Parser Param
param = do
    name <- symbol
    type_ <- typeValue
    return (name, type_)