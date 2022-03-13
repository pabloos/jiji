

module Semantic.SymbolTable.CheckSpec where

import Test.Hspec

import qualified Parser.AST as AST
import qualified Semantic.SymbolTable.Symbol as Symbol

import qualified Semantic.SymbolTable.Check as Check

spec :: Spec
spec =
    describe "symbol table check" $ do
        it "it's ok" $ do
            Check.check [Symbol.Record "a" (Symbol.Const (AST.IntVal 1))]
            `shouldBe` Nothing
        it "const already defined" $ do
            Check.check [
                Symbol.Record "a" $ Symbol.Const (AST.IntVal 1),
                Symbol.Record "a" $ Symbol.Const (AST.IntVal 1)
               ]
            `shouldBe` Just (Check.AlreadyDefined "a")
        it "subprogram already defined" $ do
            Check.check [
                Symbol.Record "a" $ Symbol.Subprogram Nothing [] [],
                Symbol.Record "a" $ Symbol.Subprogram Nothing [] []
               ]
            `shouldBe` Just (Check.AlreadyDefined "a")
        it "variables in subprogram" $ do
            Check.check [
                Symbol.Record "main" $ Symbol.Subprogram Nothing [] [],
                Symbol.Record "a" $ Symbol.Subprogram Nothing [] [
                    Symbol.Record "a" $ Symbol.Variable AST.IntType,
                    Symbol.Record "b" $ Symbol.Variable AST.StringType,
                    Symbol.Record "a" $ Symbol.Variable AST.BoolType
                ]
                ]
            `shouldBe` Just (Check.AlreadyDefined "a")
        it "variables in subprogram among other subprograms" $ do
            Check.check [
                Symbol.Record "main" $ Symbol.Subprogram Nothing [] [],
                Symbol.Record "a" $ Symbol.Subprogram Nothing [] [
                    Symbol.Record "a" $ Symbol.Variable AST.IntType,
                    Symbol.Record "b" $ Symbol.Variable AST.StringType,
                    Symbol.Record "a" $ Symbol.Variable AST.BoolType
                ],
                Symbol.Record "main1" $ Symbol.Subprogram Nothing [] []
               ]
            `shouldBe` Just (Check.AlreadyDefined "a")