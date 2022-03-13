

module Semantic.Analizer.MainCheckerSpec where

import Test.Hspec

import Parser.AST as AST

import qualified Semantic.Analizer.MainChecker as Checker

-- spec :: Spec
-- spec =
--   describe "main" $ do
--     it "should return true" $ do
--       let ast = [AST.Subprogram "main" [] Nothing []]
--       Checker.checkMain ast `shouldBe` Nothing
--     it "should return false" $ do
--         let ast = [AST.Subprogram "main" [("param1", IntType)] Nothing []]
--         Checker.checkMain ast `shouldBe` Just Checker.NotMain