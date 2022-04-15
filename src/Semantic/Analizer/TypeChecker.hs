

module Semantic.Analizer.TypeChecker where

import qualified Data.Map as Map
import Control.Monad ( msum )

import Parser.AST

import qualified Semantic.SymbolTable.Symbol as Symbol
import Semantic.SymbolTable.Table

type TypeError = String

merge :: TypeValue -> TypeValue -> TypeValue
merge t1 t2 = if t1 == t2 then t1 else error $ "Type error: \n" ++ show t1 ++ " !=\n" ++ show t2

synth :: SymbolTable -> Expr -> TypeValue
-- Literals
synth st (Lit (IntVal _)) = IntType
synth st (Lit (BoolVal _)) = BoolType
synth st (Lit (StringVal _)) = StringType
-- Int types
synth st (Add e1 e2) = merge IntType $ merge (synth st e1) (synth st e2)
synth st (Sub e1 e2) = merge IntType $ merge (synth st e1) (synth st e2)
synth st (Mul e1 e2) = merge IntType $ merge (synth st e1) (synth st e2)
synth st (Negate e) = merge IntType $ synth st e
-- Bool types
synth st (Not e) = merge BoolType $ synth st e
synth st (And e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Or e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Ne e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Eq e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Lt e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Gt e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Le e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)
synth st (Ge e1 e2) = merge BoolType $ merge (synth st e1) (synth st e2)

synth st (Call name args) = case checkCallExpr st (Call name args) of
                            Just e -> error $ "Type error: " ++ show (Call name args)
                            Nothing -> case Map.lookup name st of
                                Just (Symbol.Subprogram (Just returnType) _) -> returnType
                                _ -> error $ "Type error: function " ++ name ++ " not found"

synth st (SymbolRef name) = case Map.lookup name st of
                                Just (Symbol.Variable varType) -> varType
                                Just (Symbol.Const x) -> case x of
                                                        (IntVal _) -> IntType
                                                        (BoolVal _) -> BoolType
                                                        (StringVal _) -> StringType
                                _ -> error $ "Type error: " ++ name ++ " not found"

data CallError = TooFewArguments
                | TooManyArguments
                | InvalidArgType String
                | UnknownFunction String
                deriving (Show, Eq)

checkCallExpr :: SymbolTable -> Expr -> Maybe CallError
checkCallExpr st (Call name args) =
    case Map.lookup name st of
        Just (Symbol.Subprogram _ params) ->
            if length params == length args then checkTypes st params args
            else if length params < length args then Just TooManyArguments
            else Just TooFewArguments
        x -> Just $ UnknownFunction $ show x
checkCallExpr _ _ = Nothing

checkTypes :: SymbolTable -> [Param] -> [Expr] -> Maybe CallError
checkTypes _ [] [] = Nothing
checkTypes st params args = msum $ zipWith (sameType st) params args
                where sameType st (name, type_) expr = if synth st expr == type_ then Nothing
                                                    else Just $ InvalidArgType $ "the expression " <> show expr <> " used as an argument" <> " it's not of the type of the parameter" <> name <> ", with type: " <> show type_