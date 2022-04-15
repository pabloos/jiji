

module Semantic.Analizer.SubprogramCheckerSpec where

import Test.Hspec

import qualified Data.Map as Map

import qualified Parser.AST as AST

import qualified Semantic.SymbolTable.Table as ST
import qualified Semantic.Analizer.SubprogramChecker as Checker

spec :: Spec
spec =
    describe "subrprogram check" $ do
        it "OK: checks no return" $ do
            let subprogram = AST.Subprogram "test" [] Nothing []
            Checker.checkSubprogram Map.empty subprogram `shouldBe` Nothing

        it "OK: hecks return" $ do
            let subprogram = AST.Subprogram "test" [] (Just AST.IntType) [AST.Return (AST.Lit $ AST.IntVal 0)]
            Checker.checkSubprogram Map.empty subprogram `shouldBe` Nothing

        it "Error: return lack" $ do
            let subprogram = AST.Subprogram "test" [] (Just AST.IntType) []
            Checker.checkSubprogram Map.empty subprogram `shouldBe` Just Checker.ReturnNeeded

        it "Error: return not needed" $ do
            let subprogram = AST.Subprogram "test" [] Nothing [AST.Return (AST.Lit $ AST.IntVal 0)]
            Checker.checkSubprogram Map.empty subprogram `shouldBe` Just Checker.ReturnNotNeeded

        it "Error: return type missmatch" $ do
            let subprogram = AST.Subprogram "test" [] (Just AST.IntType) [AST.Return (AST.Lit $ AST.BoolVal True)]
            Checker.checkSubprogram Map.empty subprogram `shouldBe` Just Checker.ReturnTypeMissmatch