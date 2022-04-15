
module Parser.Statement where

import Control.Applicative

import Parser.Combinators
import Parser.Tokens
import Parser.AST
import Parser.Expr
import Parser.Type

statement :: Parser Statement
statement = printStmt <|> call <|> varAssign <|> varDeclaration <|> returnStmt <|> parseIfElse <|> ifStmt <|> whileStmt

printStmt :: Parser Statement 
printStmt = do
    reserved "print"
    expr <- parens expr
    return $ Print expr

varDeclaration :: Parser Statement 
varDeclaration = do 
  type' <- typeValue
  name <- symbol
  expr <- expr
  return $ Var name type' expr

varAssign :: Parser Statement
varAssign = do
  name <- symbol
  reserved "="
  expr <- expr
  return $ Assign name expr

returnStmt :: Parser Statement
returnStmt = do
  reserved tokenReturn
  expr <- expr
  optional $ reserved tokenSemiColon
  return $ Return expr

ifStmt :: Parser Statement
ifStmt = do
  reserved "if"
  cond <- parens expr
  then_ <- stmtBlock
  return $ If cond then_

parseIfElse :: Parser Statement
parseIfElse = do
  reserved "if"
  cond <- parens expr
  then_ <- stmtBlock
  reserved "else"
  else_ <- stmtBlock
  return $ IfElse cond then_ else_

whileStmt :: Parser Statement
whileStmt = do
  reserved "while"
  cond <- parens expr
  body <- stmtBlock
  return $ While cond body

stmtBlock :: Parser [Statement]
stmtBlock = braces $ many statement

call :: Parser Statement
call = do
  ident <- symbol
  args <- args
  return $ CallStmt ident args