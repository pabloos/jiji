module Semantic.SymbolTable.Symbol where

import Parser.AST

import Data.Map

data Symbol = Const Literal 
            | Subprogram ReturnType [Param]
            | Variable TypeValue
            deriving (Show, Eq)

type ReturnType = Maybe TypeValue