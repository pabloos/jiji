module Semantic.Result where

import Parser.AST
import Semantic.SymbolTable.Symbol
import qualified Semantic.SymbolTable.SymbolTable as ST

data Result = Result AST SymbolTable

build :: AST -> Result
build ast = Result ast $ ST.build ast