

module Semantic.SymbolTable.Table where

-- import qualified Control.Monad.State as State

import Data.Maybe (mapMaybe)
import Data.Either
import qualified Data.Map as Map

import qualified Parser.AST as AST
import qualified Semantic.SymbolTable.Symbol as Symbol

data Error = SymbolAlreadyDefined
            | SymbolNotFound String
            | TypeMissmatch String
            | SymbolNotVariable String
            deriving (Eq, Show)

type SymbolTable = Map.Map String Symbol.Symbol

insert :: String -> Symbol.Symbol -> SymbolTable -> Either SymbolTable Error
insert key symbol st = case Map.lookup key st of 
    Nothing -> Left $ Map.insert key symbol st
    Just _ -> Right SymbolAlreadyDefined

addGlobals :: SymbolTable -> [AST.Definition] -> Either SymbolTable Error
addGlobals _ [] = Left Map.empty
addGlobals symbolTable (x:xs) = case x of
    AST.Constant name value -> case insert name (Symbol.Const value) symbolTable of -- case add symbolTable $ Symbol.Record name $ Symbol.Const value of
        Left symbolTable -> addGlobals symbolTable xs
        Right e -> Right e

    AST.Subprogram "main" params maybeType sts -> addGlobals symbolTable xs -- ignore main

    AST.Subprogram name params maybeType sts -> case insert name (Symbol.Subprogram maybeType params) symbolTable of-- case add symbolTable $ Symbol.Record name $ Symbol.Subprogram maybeType params of
        Left symbolTable -> addGlobals symbolTable xs
        Right e -> Right e