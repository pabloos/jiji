module Main where

import Test.Hspec

import qualified Parser.ExprSpec as Expr
import qualified Parser.TypeSpec as Type
import qualified Parser.StmtSpec as Stmt
import qualified Parser.DefinitionSpec as Def
import qualified Parser.ProgramSpec as Program

import qualified Semantic.SymbolTable.BuildSpec as Build
import qualified Semantic.SymbolTableSpec as SymbolTable
import qualified Semantic.SymbolTable.CheckSpec as Check

import qualified Semantic.Analizer.MainCheckerSpec as MainChecker


main :: IO ()
main = hspec $ Expr.spec 
            >> Type.spec
            >> Stmt.spec
            >> Def.spec
            >> Program.spec
            >> SymbolTable.spec
            >> Build.spec
            >> Check.spec
            >> MainChecker.spec