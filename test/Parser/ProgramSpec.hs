module Parser.ProgramSpec where


import Test.Hspec

import Parser.Combinators
import Parser.AST
import Parser.Program

spec :: Spec
spec =
    describe "Program parser" $ do
        it "parses empty main" $ do
            runParser program "fun main() {}" `shouldBe` [Subprogram "main" [] Nothing []]
        it "parses main and one more procedure" $ do
            runParser program "fun main() {} fun foo() {}" `shouldBe` [Subprogram "main" [] Nothing [], Subprogram "foo" [] Nothing []]
        it "parses procedures and constants" $ do
            runParser program "fun main() {} const a = 1" `shouldBe` [Subprogram "main" [] Nothing [], Constant "a" (IntVal 1)]
        it "parses procedures and functions" $ do
            runParser program "fun main() {} fun foo() -> int {}" `shouldBe` [Subprogram "main" [] Nothing [], Subprogram "foo" [] (Just IntType) []]