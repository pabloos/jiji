module Semantic.Analysis where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad ( msum )

import Parser.AST

import Semantic.SymbolTable.Symbol
import qualified Semantic.SymbolTable.Table as ST
import qualified Semantic.Analizer.MainChecker as MainChecker
import qualified Semantic.Analizer.SubprogramChecker as SubprogramChecker

data Error = SubError SubprogramChecker.Error
            | MainError MainChecker.Error
            | GlobalError String
            deriving (Show, Eq)

analysis :: AST -> Maybe Error
analysis ast = do
                case ST.addGlobals Map.empty ast of 
                    Right err -> Just $ GlobalError $ show err
                    Left st -> do                        
                        let subprogramResult = msum (map (SubprogramChecker.checkSubprogram st) ast)
                        let mainResult = MainChecker.checkMain ast
                        if isJust subprogramResult then Just $ SubError $ fromJust subprogramResult
                        else 
                            if isJust mainResult then Just $ MainError $ fromJust mainResult
                            else Nothing