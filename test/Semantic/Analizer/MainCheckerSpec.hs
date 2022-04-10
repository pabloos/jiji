

module Semantic.Analizer.MainCheckerSpec where

import Test.Hspec

import Parser.AST as AST

import qualified Semantic.Analizer.MainChecker as Checker

spec :: Spec
spec =
  describe "main" $ do
    it "Ok" $ do
        let ast = [AST.Subprogram "main" [] Nothing []]
        Checker.checkMain ast `shouldBe` Nothing

    it "No Main" $ do
        let ast = [AST.Subprogram "nomain" [("param1", IntType)] Nothing []]
        Checker.checkMain ast `shouldBe` Just Checker.NoMain

    it "Main with type" $ do
        let ast = [AST.Subprogram "main" [] (Just IntType) []]
        Checker.checkMain ast `shouldBe` Just Checker.TypedMain

    it "Main with return" $ do
        let ast = [AST.Subprogram "main" [] Nothing [AST.Return (AST.Lit (AST.IntVal 0))]]
        Checker.checkMain ast `shouldBe` Just Checker.ReturnInMain

    it "Main with params" $ do
        let ast = [AST.Subprogram "main" [("param1", IntType)] Nothing []]
        Checker.checkMain ast `shouldBe` Just Checker.NotEmptyParams
