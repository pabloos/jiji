module Semantic.SymbolTable.Build where

import Test.Hspec

import Parser.AST as AST
import Semantic.SymbolTable.Symbol as Symbol
import Semantic.SymbolTable.SymbolTable

spec :: Spec
spec =
    describe "build symbol table" $ do
        describe "add subprograms" $ do
            it "should add an empty subprogram" $ do
                add $ AST.Subprogram "a" [] Nothing []
                `shouldBe`
                Symbol.Record "a" (Symbol.Subprogram Nothing [] [])

            it "should add a new subprogram" $ do
                add $ AST.Subprogram "a" [("b", AST.IntType)] Nothing []
                `shouldBe`
                Symbol.Record "a" (Symbol.Subprogram Nothing [("b", AST.IntType)] [])

            it "should add a new function" $ do
                add $ AST.Subprogram "a" [("b", AST.IntType)] (Just AST.IntType) []
                `shouldBe`
                Symbol.Record "a" (Symbol.Subprogram (Just AST.IntType) [("b", AST.IntType)] [])
            
            it "should add a new function with some params" $ do
                add $ AST.Subprogram "a" [("b", AST.IntType), ("c", AST.IntType)] (Just AST.IntType) []
                `shouldBe`
                Symbol.Record "a" (Symbol.Subprogram (Just AST.IntType) [("b", AST.IntType), ("c", AST.IntType)] [])

            it "should add a new function with one var" $ do
                add $ AST.Subprogram "a" [("b", AST.IntType)] (Just AST.IntType) [AST.Var "c" AST.IntType $ AST.Lit (AST.IntVal 1)]
                `shouldBe`
                Symbol.Record "a" (Symbol.Subprogram (Just AST.IntType) [("b", AST.IntType)] [
                    Symbol.Record "c" (Symbol.Variable AST.IntType)
                ])

            it "should add a new function with some vars" $ do
                add $ AST.Subprogram "a" [("b", AST.IntType)] (Just AST.IntType) [
                    AST.Var "c" AST.IntType $ Lit (IntVal 1), 
                    AST.Var "d" AST.IntType $ Lit (IntVal 2)]
                `shouldBe`                
                Symbol.Record "a" (Symbol.Subprogram (Just AST.IntType) [("b", AST.IntType)] [
                    Symbol.Record "c" (Symbol.Variable AST.IntType), 
                    Symbol.Record "d" (Symbol.Variable AST.IntType)
                ])

        describe "add constants" $ do
            it "should add a new integer constant" $ do
                add $ AST.Constant "a" $ AST.IntVal 1
                `shouldBe`
                Symbol.Record "a" (Symbol.Const (AST.IntVal 1))

            it "should add a new bool constant" $ do
                add $ AST.Constant "a" $ AST.BoolVal True
                `shouldBe`
                Symbol.Record "a" (Symbol.Const (AST.BoolVal True))

            it "should add a new string constant" $ do
                add $ AST.Constant "a" $ AST.StringVal "a"
                `shouldBe`
                Symbol.Record "a" (Symbol.Const (AST.StringVal "a"))