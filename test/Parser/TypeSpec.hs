module Parser.TypeSpec where

import Test.Hspec

import Parser.Combinators
import Parser.AST
import Parser.Type

spec :: Spec
spec = 
    describe "type values" $ do
        it "parses a type value" $ do
            runParser typeValue "int" `shouldBe` IntType
            runParser typeValue "bool" `shouldBe` BoolType
            runParser typeValue "string" `shouldBe` StringType
