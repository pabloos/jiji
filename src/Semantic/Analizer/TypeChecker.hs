

module Semantic.Analizer.TypeChecker where

import Parser.AST

import Semantic.Result
import qualified Semantic.SymbolTable.Symbol as Symbol
import Semantic.SymbolTable.SymbolTable

type TypeError = String

-- typeCheck :: Result -> Maybe String
-- typeCheck (Result ast st) [defs] = Nothing
-- typeCheck _ _ = Just "Type error" 

merge :: TypeValue -> TypeValue -> TypeValue
merge t1 t2 = if t1 == t2 then t1 else error $ "Type error: \n" ++ show t1 ++ " !=\n" ++ show t2

walk :: Symbol.SymbolTable -> Expr -> TypeValue
-- Literals
walk st (Lit (IntVal _)) = IntType
walk st (Lit (BoolVal _)) = BoolType
walk st (Lit (StringVal _)) = StringType
-- Int types
walk st (Add e1 e2) = merge IntType $ merge (walk st e1) (walk st e2)
walk st (Sub e1 e2) = merge IntType $ merge (walk st e1) (walk st e2)
walk st (Mul e1 e2) = merge IntType $ merge (walk st e1) (walk st e2)
walk st (Negate e) = merge IntType $ walk st e
-- Bool types
walk st (Not e) = merge BoolType $ walk st e
walk st (And e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Or e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Ne e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Eq e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Lt e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Gt e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Le e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)
walk st (Ge e1 e2) = merge BoolType $ merge (walk st e1) (walk st e2)

walk st (Call funcName _) = case search funcName st of
                                Just (Symbol.Record _ (Symbol.Subprogram (Just returnType) _ _)) -> returnType
                                _ -> error $ "Type error: function " ++ funcName ++ " not found"

walk st (SymbolRef name) = case search name st of
                                Just (Symbol.Record _ (Symbol.Variable varType)) -> varType
                                Just (Symbol.Record _ (Symbol.Const ((IntVal _)))) -> IntType
                                Just (Symbol.Record _ (Symbol.Const ((BoolVal _)))) -> BoolType
                                Just (Symbol.Record _ (Symbol.Const ((StringVal _)))) -> StringType
                                _ -> error $ "Type error: " ++ name ++ " not found"