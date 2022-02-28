
module Parser.Statement where

import Control.Applicative

import Parser.Combinators
import Parser.Tokens
import Parser.AST
import Parser.Expr

statement :: Parser Statement
statement = printStmt <|> parseLetStmt <|> parseReturnStmt <|> parseIfElse <|> parseIfStmt <|> parseWhileStmt <|> parseAssignStmt

printStmt :: Parser Statement 
printStmt = do
    reserved "print"
    expr <- expr
    return $ Print expr

parseLetStmt :: Parser Statement
parseLetStmt = do
  ident <- ident
  reserved tokenAssign
  expr <- expr
  optional $ string tokenReturn
  return $ Assign ident expr

parseReturnStmt :: Parser Statement
parseReturnStmt = do
  string tokenReturn
  expr <- expr
  optional $ reserved tokenSemiColon
  return $ Return expr

parseIfStmt :: Parser Statement
parseIfStmt = do
  reserved "if"
  cond <- expr
  reserved "then"
  then_ <- many statement
  return $ If cond then_

parseIfElse :: Parser Statement
parseIfElse = do
  reserved "if"
  cond <- expr
  reserved "then"
  then_ <- many statement
  reserved "else"
  else_ <- many statement
  return $ IfElse cond then_ else_

parseWhileStmt :: Parser Statement
parseWhileStmt = do
  reserved "while"
  cond <- expr
  reserved "do"
  body <- many statement
  return $ While cond body

parseAssignStmt :: Parser Statement
parseAssignStmt = do
  ident <- ident
  reserved tokenAssign
  expr <- expr
  optional $ reserved tokenSemiColon
  return $ Assign ident expr