

module Semantic.Analizer.MainChecker where

import Parser.AST

data MainException = NotEmptyParams
                    | NotMain
                    | TypedMain
                    | ReturnInMain
                    deriving (Show, Eq)

checkMain :: AST -> Maybe MainException
checkMain ast = do
                    noreturn (statements $ getMain ast)

getMain :: AST -> Definition
getMain [] = error "No main"
getMain (x:xs) = case x of
  Subprogram "main" [] Nothing _ -> x
  _ -> getMain xs

noreturn :: [Statement] -> Maybe MainException
noreturn [] = Nothing
noreturn (x:xs) = case x of
  Return _ -> Just ReturnInMain
  _ -> noreturn xs

statements :: Definition -> [Statement]
statements (Subprogram _ _ Nothing stmts) = stmts
statements _ = []