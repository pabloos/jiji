module Parser.Definition where

import Control.Applicative

import Parser.Expr
import Parser.AST
import Parser.Combinators
import Parser.Statement

definition :: Parser Definition
definition =
    -- ConstInt <$> symbol <*> literal <|>
    -- ConstString <$> symbol <*> parseString <|>
    -- ConstBool <$> symbol <*> literal <|>
    Constant <$> symbol <*> expr <|>
    Var <$> symbol <*> expr <|>
    Fun <$> symbol <*> args <*> many statement
