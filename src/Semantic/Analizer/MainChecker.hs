

module Semantic.Analizer.MainChecker where

import Control.Monad ( msum )

import Parser.AST

data MainException = NotEmptyParams
                    | NoMain
                    | TypedMain
                    | ReturnInMain
                    | Unknown String
                    deriving (Show, Eq)

checkMain :: AST -> Maybe MainException
checkMain ast = case getMain ast of
                Nothing -> Just NoMain
                Just main -> do 
                  let params = noParams main
                  let typed = withType main
                  let ret = noreturn (statements main)
                  msum [params, typed, ret] 

getMain :: AST -> Maybe Definition
getMain [] = Nothing
getMain (x:xs) = case x of
  Subprogram "main" _ _ _ -> Just x
  _ -> getMain xs

noParams :: Definition -> Maybe MainException
noParams (Subprogram "main" [] _ _) = Nothing
noParams (Subprogram "main" x _ _) = Just NotEmptyParams
noParams _ = Just $ Unknown "unkown subprogram"

withType :: Definition -> Maybe MainException
withType (Subprogram "main" _ (Just _) _) = Just TypedMain
withType _ = Nothing

noreturn :: [Statement] -> Maybe MainException
noreturn [] = Nothing
noreturn (x:xs) = case x of
  Return _ -> Just ReturnInMain
  _ -> noreturn xs

statements :: Definition -> [Statement]
statements (Subprogram _ _ Nothing stmts) = stmts
statements _ = []