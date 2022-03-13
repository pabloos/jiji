module Parser.DefinitionSpec where

import Test.Hspec

import Parser.Combinators
import Parser.AST
import Parser.Definition


spec :: Spec
spec = 
    describe "Parser.Definition" $ do
        describe "Constant definition" $ do
            it "parses a constant definition" $ do
                runParser constant "const a = 1" `shouldBe` Constant "a" (IntVal 1)
            it "parses a constant definition without spaces" $ do
                runParser constant "const a=1" `shouldBe` Constant "a" (IntVal 1)
        
        describe "procedure definition" $ do
            it "parses an empty procedure" $ do
                runParser procedure "fun a() {}" `shouldBe` Subprogram "a" [] Nothing []
            it "parses a procedure with a single argument" $ do
                runParser procedure "fun a(b string) {}" `shouldBe` Subprogram "a" [("b", StringType)] Nothing []
            it "parses a procedure definition with a statement" $ do
                runParser procedure "fun a() { print(2) }" `shouldBe` Subprogram "a" [] Nothing [Print (Lit (IntVal 2))]
        
        describe "function definition" $ do
            it "parses an empty function" $ do
                runParser function "fun a() -> string {}" `shouldBe` Subprogram "a" [] (Just StringType) []
            it "parses a function with a single argument" $ do
                runParser function "fun a(b string) -> string {}" `shouldBe` Subprogram "a" [("b", StringType)] (Just StringType) []
            it "parses a function definition with a statement" $ do
                runParser function "fun a() -> string { print(2) }" `shouldBe` Subprogram "a" [] (Just StringType) [Print (Lit (IntVal 2))]
            it "parses a function definition with a statement and a return statement" $ do
                runParser function "fun a() -> string { print(2)\n return \"hello\" }" `shouldBe` Subprogram "a" [] (Just StringType) [Print (Lit (IntVal 2)), Return (Lit (StringVal "hello"))]