

module Semantic.Analizer.CallCheckerSpec where

import Test.Hspec

import qualified Data.Map as Map

import Parser.AST as AST
import Semantic.SymbolTable.Symbol as Symbol
import Semantic.Analizer.TypeChecker

spec :: Spec
spec =
    describe "call checks" $ do
        it "OK: empty params and args" $ do
            let st = Map.fromList [("c", Symbol.Const $ AST.IntVal 3), ("a", Symbol.Subprogram (Just AST.IntType) [])]
            let call = AST.Call "a" []
            checkCallExpr st call `shouldBe` Nothing

        it "OK: 1:1 params and args" $ do
            let st = Map.fromList [("c", Symbol.Const $ AST.IntVal 3), ("a", Symbol.Subprogram (Just AST.IntType) [("x", AST.IntType)])]
            let call = AST.Call "a" [AST.Lit $ AST.IntVal 1]
            checkCallExpr st call `shouldBe` Nothing

        it "TooManyArguments Error: 1:m params and args" $ do
            let st = Map.fromList [("c", Symbol.Const $ AST.IntVal 3), ("a", Symbol.Subprogram (Just AST.IntType) [("x", AST.IntType)])]
            let call = AST.Call "a" [AST.Lit $ AST.IntVal 1, AST.Lit $ AST.IntVal 2]
            checkCallExpr st call `shouldBe` Just TooManyArguments

        it "TooFewArguments Error: 1:0 params and args" $ do
            let st =  Map.fromList [("c", Symbol.Const $ AST.IntVal 3), ("a", Symbol.Subprogram (Just AST.IntType) [("x", AST.IntType)])]
            let call = AST.Call "a" []
            checkCallExpr st call `shouldBe` Just TooFewArguments

        it "InvalidParamArg Error: invalid argument type" $ do
            let st = Map.fromList [("c", Symbol.Const $ AST.IntVal 3), ("a", Symbol.Subprogram (Just AST.IntType) [("x", AST.IntType)])]
            let call = AST.Call "a" [AST.Lit $ AST.BoolVal True]
            checkCallExpr st call `shouldBe` Just (InvalidArgType "the expression Lit (BoolVal True) used as an argument it's not of the type of the parameterx, with type: IntType")