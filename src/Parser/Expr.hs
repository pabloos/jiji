module Parser.Expr where

import Control.Applicative
import Parser.Combinators
import Parser.AST

expr :: Parser Expr
expr = equality

equality :: Parser Expr
equality = chainl1 comparison (infixOp "==" Eq <|> infixOp "!=" Neq)

comparison :: Parser Expr
comparison = chainl1 term (infixOp "<" Lt <|> infixOp ">" Gt <|> infixOp "<=" Le <|> infixOp ">=" Ge)

term :: Parser Expr
term = chainl1 factor (infixOp "-" Sub <|> infixOp "+" Add)

factor :: Parser Expr
factor = chainl1 unary (infixOp "*" Mul)

unary :: Parser Expr
unary = Negate <$> (char '-' *> primary) <|> primary

primary :: Parser Expr
primary = parseCallExpr <|> parens expr <|> intExpr <|> bool <|> stringExpr <|> parseIdentExpr

bool :: Parser Expr
bool = BoolLit <$> (True <$ reserved "true" <|> False <$ reserved "false")

intExpr :: Parser Expr
intExpr = IntLit <$> int

stringExpr :: Parser Expr
stringExpr = StringLit <$> stringLit

stringLit :: Parser String
stringLit = char '"' *> many (noneOf "\"") <* char '"'

args :: Parser [Expr]
args = parens $ sepBy expr (reserved ",")

symbol :: Parser String
symbol = many1 (oneOf ['a'..'z']) -- except ...

parseIdentExpr :: Parser Expr
parseIdentExpr = SymbolRef <$> symbol

parseCallExpr :: Parser Expr
parseCallExpr = do
  ident <- symbol
  args <- args
  return $ Call ident args