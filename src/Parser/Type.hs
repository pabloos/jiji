module Parser.Type where

import Control.Applicative

import Parser.AST
import Parser.Combinators

typeValue :: Parser TypeValue
typeValue = (reserved "int" >> return IntType)
        <|> (reserved "bool" >> return BoolType)
        <|> (reserved "string" >> return StringType)