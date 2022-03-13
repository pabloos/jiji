module Semantic.SymbolTable.Symbol where

import Parser.AST

data Symbol = Record String SymbolType deriving (Show, Eq)

type SymbolTable = [Symbol]

data SymbolType = Const Literal 
                | Subprogram ReturnType [Param] SymbolTable
                | Variable TypeValue
                deriving (Show, Eq)

type ReturnType = Maybe TypeValue

name :: Symbol -> String
name (Record n _) = n

-- type_ :: Symbol -> Maybe TypeValue
-- type_ (Record _ t _ _) = t

symbolType :: Symbol -> SymbolType
symbolType (Record _ t) = t