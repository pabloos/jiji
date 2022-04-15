module Main where

import Test.Hspec

import qualified Parser.ExprSpec as Expr
import qualified Parser.TypeSpec as Type
import qualified Parser.StmtSpec as Stmt
import qualified Parser.DefinitionSpec as Def
import qualified Parser.ProgramSpec as Program

import qualified Semantic.Analizer.MainCheckerSpec as MainChecker
import qualified Semantic.Analizer.CallCheckerSpec as CallChecker
import qualified Semantic.Analizer.SubprogramCheckerSpec as ReturnChecker
import qualified Semantic.Analizer.StatementCheckerSpec as StatementChecker

main :: IO ()
main = hspec $ Expr.spec 
            >> Type.spec
            >> Stmt.spec
            >> Def.spec
            >> Program.spec
            >> MainChecker.spec
            >> CallChecker.spec
            >> ReturnChecker.spec
            >> StatementChecker.spec