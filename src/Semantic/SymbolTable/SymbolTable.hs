module Semantic.SymbolTable.SymbolTable where

-- import qualified Control.Monad.State as State
-- import qualified Data.Map as Map

import Data.Maybe (mapMaybe)

import qualified Parser.AST as AST
import qualified Semantic.SymbolTable.Symbol as Symbol

build :: AST.AST -> Symbol.SymbolTable
build = map add

search :: String -> Symbol.SymbolTable -> Maybe Symbol.Symbol
search symbolName (x:xs) = if match symbolName x then Just x else search symbolName xs
search _ [] = Nothing

match :: String -> Symbol.Symbol -> Bool
match symbolName (Symbol.Record n _) = symbolName == n

add :: AST.Definition -> Symbol.Symbol
add (AST.Constant name value) = Symbol.Record name (Symbol.Const value)
add (AST.Subprogram name params maybeType sts) = Symbol.Record name (Symbol.Subprogram maybeType params (mapMaybe addVar sts))

addVar :: AST.Statement -> Maybe Symbol.Symbol
addVar (AST.Var name type_ _) = Just $ Symbol.Record name $ Symbol.Variable type_
addVar _ = Nothing