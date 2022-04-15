

module Semantic.Analizer.SubprogramChecker where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad ( msum )
import Control.Applicative ( Alternative((<|>)) )

import qualified Parser.AST as AST

import qualified Semantic.SymbolTable.Symbol as Symbol
import qualified Semantic.SymbolTable.Table as ST

import qualified Semantic.Analizer.TypeChecker as Type

data Error = ReturnNeeded
            | ReturnNotNeeded
            | ReturnTypeMissmatch
            | TooManyArguments
            | TooFewArguments
            | VarTypeMissmatch
            | CondNotBoolEval
            | InvalidArgType String
            | VarAlreadyDeclared
            | SymbolTableError ST.Error
            | UnknownFunction String
            | VarNotDeclared
            deriving (Eq, Show)

checkSubprogram :: ST.SymbolTable -> AST.Definition -> Maybe Error
checkSubprogram st (AST.Subprogram _ params type_ stmts) = do 
                                    let topST = st `Map.union` stFromParams params
                                    case type_ of
                                        Nothing -> if hasReturn stmts then Just ReturnNotNeeded
                                                    else checkStatements topST stmts
                                        Just type_ -> if hasReturn stmts 
                                                        then checkReturns topST type_ stmts <|> checkStatements topST stmts
                                                        else Just ReturnNeeded                    
checkSubprogram st _ = Nothing

hasReturn :: [AST.Statement] -> Bool
hasReturn [] = False
hasReturn (x:xs) = case x of
    (AST.Return _) -> True
    _ -> hasReturn xs

checkReturnType :: ST.SymbolTable -> AST.TypeValue -> AST.Statement -> Maybe Error
checkReturnType st type_ (AST.Return expr) = checkType type_ ReturnTypeMissmatch st expr
checkReturnType st type_ (AST.If expr stmts) = checkReturns st type_ stmts
checkReturnType st type_ (AST.IfElse expr stmts1 stmts2) = msum $ map (checkReturns st type_) [stmts1, stmts2]
checkReturnType st type_ (AST.While expr stmts) = checkReturns st type_ stmts
checkReturnType _ _ _ = Nothing

checkReturns :: ST.SymbolTable -> AST.TypeValue -> [AST.Statement] -> Maybe Error
checkReturns _ _ [] = Nothing
checkReturns st type_ stmts = msum $ map (checkReturnType st type_) stmts

stFromParams :: [AST.Param] -> ST.SymbolTable
stFromParams [] = Map.empty
stFromParams ((name, type_):xs) = Map.singleton name (Symbol.Variable type_) `Map.union` stFromParams xs

checkStatements :: ST.SymbolTable -> [AST.Statement] -> Maybe Error
checkStatements _ [] = Nothing
checkStatements st (x:xs) = case x of

    (AST.CallStmt name args) -> case Map.lookup name st of
        Just (Symbol.Subprogram _ params) ->
            if length params == length args then checkTypes_ st params args
            else if length params < length args then Just TooManyArguments
            else Just TooFewArguments
        x -> Just $ UnknownFunction $ show x

    (AST.Var name type_ expr) -> case checkType type_ VarTypeMissmatch st expr of
        Nothing -> case ST.insert name (Symbol.Variable type_) st of
            Right err -> Just $ SymbolTableError err
            Left newSt -> if type_ == Type.synth st expr
                            then checkStatements st xs
                            else Just $ SymbolTableError $ ST.TypeMissmatch name
        Just e -> Just e

    (AST.Assign name expr) -> case Map.lookup name st of
        Nothing -> Just $ SymbolTableError $ ST.SymbolNotFound name
        Just symbol -> case symbol of
            (Symbol.Variable type_) -> case checkType type_ VarTypeMissmatch st expr of
                                        Nothing -> checkStatements st xs
                                        Just e -> Just e
            _ -> Just $ SymbolTableError $ ST.SymbolNotVariable name

    (AST.If expr stmts) -> case checkBool st expr of
        Just e -> Just e
        Nothing -> case checkStatements st stmts of
            Just e -> Just e
            Nothing -> checkStatements st xs

    (AST.IfElse expr stmts1 stmts2) -> case checkBool st expr of
        Just e -> Just e
        Nothing -> case msum $ (map $ checkStatements st) [stmts1, stmts2] of
            Just e -> Just e
            Nothing -> checkStatements st xs

    (AST.While expr stmts) -> case checkBool st expr of
        Just e -> Just e
        Nothing -> case checkStatements st stmts of
            Just e -> Just e
            Nothing -> checkStatements st xs

    _ -> checkStatements st xs

checkBool :: ST.SymbolTable -> AST.Expr -> Maybe Error
checkBool = checkType AST.BoolType CondNotBoolEval

checkType :: AST.TypeValue -> Error -> ST.SymbolTable -> AST.Expr -> Maybe Error
checkType type_ err st expr = if type_ == Type.synth st expr then Nothing else Just err

checkTypes_ :: ST.SymbolTable -> [AST.Param] -> [AST.Expr] -> Maybe Error
checkTypes_ _ [] [] = Nothing
checkTypes_ st params args = msum $ zipWith (sameType st) params args
                where sameType st (name, type_) expr = if Type.synth st expr == type_ then Nothing
                                                    else Just $ InvalidArgType $ "the expression " <> show expr <> " used as an argument" <> " it's not of the type of the parameter" <> name <> ", with type: " <> show type_