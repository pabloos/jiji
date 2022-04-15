

module Semantic.Analizer.StatementCheckerSpec where

import Test.Hspec

import qualified Data.Map as Map

import qualified Parser.AST as AST

import qualified Semantic.Analizer.SubprogramChecker as Checker
import qualified Semantic.SymbolTable.Symbol as Symbol
import qualified Semantic.SymbolTable.Table as ST

spec :: Spec
spec = 
    describe "StatementChecker" $ do
        it "OK: var stmt" $ do
            let st = Map.empty
            let stmt = [AST.Var "a" AST.IntType (AST.Lit $ AST.IntVal 1)]
            Checker.checkStatements st stmt `shouldBe` Nothing
        
        it "OK: while stmt" $ do
            let st = Map.empty
            let stmt = [AST.While (AST.Lit $ AST.BoolVal True) []]
            Checker.checkStatements st stmt `shouldBe` Nothing

        it "OK: if stmt" $ do
            let st = Map.empty
            let stmt = [AST.If (AST.Lit $ AST.BoolVal True) []]
            Checker.checkStatements st stmt `shouldBe` Nothing
        
        it "OK: if else stmt" $ do
            let st = Map.empty
            let stmt = [AST.IfElse (AST.Lit $ AST.BoolVal True) [] []]
            Checker.checkStatements st stmt `shouldBe` Nothing

        it "OK: correct assign" $ do
            let st = Map.fromList [("a", Symbol.Variable AST.IntType)]
            let stmt = [AST.Assign "a" (AST.Lit $ AST.IntVal 1)]
            Checker.checkStatements st stmt `shouldBe` Nothing

        it "Error: not a bool expr in If else" $ do
            let st = Map.empty
            let stmt = [AST.IfElse (AST.Lit $ AST.IntVal 1) [] []]
            Checker.checkStatements st stmt `shouldBe` Just Checker.CondNotBoolEval
        
        it "Error: not a bool expr in If" $ do
            let st = Map.empty
            let stmt = [AST.If (AST.Lit $ AST.IntVal 1) []]
            Checker.checkStatements st stmt `shouldBe` Just Checker.CondNotBoolEval
        
        it "Error: not a bool expr in While" $ do
            let st = Map.empty
            let stmt = [AST.While (AST.Lit $ AST.IntVal 1) []]
            Checker.checkStatements st stmt `shouldBe` Just Checker.CondNotBoolEval
        
        it "Error: var type mismatch" $ do
            let st = Map.empty
            let stmt = [AST.Var "a" AST.IntType (AST.Lit $ AST.BoolVal False)]
            Checker.checkStatements st stmt `shouldBe` Just Checker.VarTypeMissmatch

        it "Error: var not declared" $ do
            let st = Map.empty
            let stmt = [AST.Assign "a" (AST.Lit $ AST.BoolVal False)]
            Checker.checkStatements st stmt `shouldBe` Just (Checker.SymbolTableError (ST.SymbolNotFound "a"))
        
        it "Error: assign missmatched type" $ do
            let st = Map.fromList [("a", Symbol.Variable AST.IntType)]
            let stmt = [AST.Assign "a" (AST.Lit $ AST.BoolVal False)]
            Checker.checkStatements st stmt `shouldBe` Just Checker.VarTypeMissmatch