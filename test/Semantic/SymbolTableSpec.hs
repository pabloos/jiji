

module Semantic.SymbolTableSpec where

import Test.Hspec

import Parser.AST as AST
import Semantic.SymbolTable.Symbol as Symbol
import Semantic.SymbolTable.SymbolTable

spec :: Spec
spec =
    describe "Symbol Table" $ do
        it "should add a constant symbol" $ do 
            build [AST.Constant "a" (AST.IntVal 1)]
            `shouldBe` 
            [
                Symbol.Record "a" (Symbol.Const (AST.IntVal 1))
            ]
        it "should add a function symbol" $ do
            build [AST.Subprogram "a" [] (Just AST.IntType) []]
            `shouldBe` 
            [
                Symbol.Record "a" (Symbol.Subprogram (Just IntType) [] [])
            ]
        it "should add a function symbol with params and some statements inside it" $ do
            build [
                AST.Subprogram "a" [("paramOne", AST.BoolType), ("paramTwo", StringType)] (Just AST.IntType) [
                    AST.If (Lit (BoolVal True)) [],
                    AST.Var "a" AST.IntType (Lit (IntVal 1)),
                    AST.If (Lit (BoolVal True)) []
                ]]
            `shouldBe` 
            [
                Symbol.Record "a" (Symbol.Subprogram (Just IntType) [("paramOne", AST.BoolType), ("paramTwo", StringType)] [
                    Symbol.Record "a" (Symbol.Variable AST.IntType)
                ])
            ]