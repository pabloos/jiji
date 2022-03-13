

module Parser.Literal where

import Control.Applicative

import Parser.AST
import Parser.Combinators

boolLiteral :: Parser Bool
boolLiteral = True <$ reserved "true" <|> False <$ reserved "false"

intLiteral :: Parser Int
intLiteral = read <$> many1 digit

stringLiteral :: Parser String
stringLiteral = quotations $ many (noneOf "\"")