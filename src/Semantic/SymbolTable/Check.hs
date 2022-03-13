

module Semantic.SymbolTable.Check where

import Semantic.SymbolTable.Symbol as Symbol

data SymbolTableError =
  AlreadyDefined String
  deriving (Eq, Show)

check :: SymbolTable -> Maybe SymbolTableError
check [] = Nothing
check (symbol:st) = case search symbol st of
                    Nothing -> check st
                    err -> err
     
search :: Symbol -> SymbolTable -> Maybe SymbolTableError
search symbol [] = case symbol of
                    Symbol.Record _ (Symbol.Subprogram _ _ st2) -> check st2
                    _ -> Nothing
search symbol (s:st) = if match symbol s then Just $ AlreadyDefined $ Symbol.name symbol
                       else search symbol st
                       where match s1 s2 = Symbol.name s1 == Symbol.name s2