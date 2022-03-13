module Parser.StmtSpec where

import Test.Hspec

import Parser.AST
import Parser.Statement
import Parser.Combinators

spec :: Spec
spec =
    describe "statement parsers" $ do
        describe "print statement" $ do
            it "parses print statement" $ do
                let input = "print(\"hello\")"
                let expected = Print (Lit (StringVal "hello"))
                let result = runParser printStmt input
                result `shouldBe` expected

            it "parses print statement with expression" $ do
                let input = "print(x)"
                let expected = Print (SymbolRef "x")
                let result = runParser printStmt input
                result `shouldBe` expected

            it "parses if" $ do
                let input = "if (x) { return y }"
                let expected = If (SymbolRef "x") [Return $ SymbolRef "y"]
                let result = runParser ifStmt input
                result `shouldBe` expected

            it "parses while" $ do
                let input = "while (x < 10) { print(x)}"
                let expected = While (Lt (SymbolRef "x") (Lit (IntVal 10))) [Print (SymbolRef "x")]
                let result = runParser whileStmt input
                result `shouldBe` expected

        -- print call needs to have an expression
        -- it "parses print statement without expression" $ do
            -- let input = "print()"
            -- let expected = Print (IntLit 0)
            -- let result = runParser printStmt input
            -- result `shouldBe` expected