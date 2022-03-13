module Parser.ExprSpec where

import Test.Hspec

import Parser.Combinators
import Parser.AST
import Parser.Expr

spec :: Spec
spec =
  describe "expr" $ do
  it "simple number" $ do
    runParser expr "2" `shouldBe` Lit (IntVal 2)

  it "simple addition (no spaces)" $ do
    runParser expr "2+3" `shouldBe` Add (Lit (IntVal 2)) (Lit (IntVal 3))

  it "simple addition with left space" $ do
    runParser expr "2 +3" `shouldBe` Add (Lit (IntVal 2)) (Lit (IntVal 3))

  it "simple addition with right space" $ do
    runParser expr "2+ 3" `shouldBe` Add (Lit (IntVal 2)) (Lit (IntVal 3))

  it "simple addition with spaces" $ do
    runParser expr "2 + 3" `shouldBe` Add (Lit (IntVal 2)) (Lit (IntVal 3))

  describe "subtraction" $ do
    it "simple subtraction" $ do
      runParser expr "2-3" `shouldBe` Sub (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple subtraction with left space" $ do
      runParser expr "2 -3" `shouldBe` Sub (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple subtraction with right space" $ do
      runParser expr "2- 3" `shouldBe` Sub (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple subtraction with spaces" $ do
      runParser expr "2 - 3" `shouldBe` Sub (Lit (IntVal 2)) (Lit (IntVal 3))
    
  describe "multiplication" $ do
    it "simple multiplication" $ do
      runParser expr "2*3" `shouldBe` Mul (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple multiplication with left space" $ do
      runParser expr "2 *3" `shouldBe` Mul (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple multiplication with right space" $ do
      runParser expr "2* 3" `shouldBe` Mul (Lit (IntVal 2)) (Lit (IntVal 3))

    it "simple multiplication with spaces" $ do
      runParser expr "2 * 3" `shouldBe` Mul (Lit (IntVal 2)) (Lit (IntVal 3))

  describe "bool literal" $ do
    it "simple bool" $ do
      runParser bool "true" `shouldBe` Lit (BoolVal True)

    it "simple bool with left space" $ do
      runParser bool " true" `shouldBe` Lit (BoolVal True)

    it "simple bool with right space" $ do
      runParser bool "false " `shouldBe` Lit (BoolVal False)

    it "simple bool with spaces" $ do
      runParser bool " false " `shouldBe` Lit (BoolVal False)

  describe "not expresion" $ do
    it "simple not" $ do
      runParser expr "!true" `shouldBe` Not (Lit (BoolVal True))

    it "simple not with left space" $ do
      runParser expr " !true" `shouldBe` Not (Lit (BoolVal True))

    it "simple not with right space" $ do
      runParser expr "! true" `shouldBe` Not (Lit (BoolVal True))

    it "simple not with spaces" $ do
      runParser expr " ! true" `shouldBe` Not (Lit (BoolVal True))

    it "not with a arithmetic expr (semantic error)" $ do
      runParser expr "!2+3" `shouldBe` Not (Add (Lit (IntVal 2)) (Lit (IntVal 3)))

    it "not not" $ do
      runParser expr "!!true" `shouldBe` Not (Not (Lit (BoolVal True)))

  describe "string literal" $ do
    it "empty string" $ do
      runParser stringExpr "\"\"" `shouldBe` Lit (StringVal "")

    it "simple string" $ do
      runParser stringExpr "\" hello \"" `shouldBe` Lit (StringVal " hello ")

  describe "int literal" $ do
    it "simple int" $ do
      runParser intExpr "2" `shouldBe` Lit (IntVal 2)
  
  describe "identifier" $ do
    it "simple symbol" $ do
      runParser parseIdentExpr "hello" `shouldBe` SymbolRef "hello"

  describe "args" $ do
    it "empty args" $ do
      runParser args "()" `shouldBe` []

    it "one arguement" $ do
      runParser args "(2)" `shouldBe` [Lit (IntVal 2)]

    it "two arguments" $ do
      runParser args "(2, 3)" `shouldBe` [Lit (IntVal 2), Lit (IntVal 3)]

    it "two arguments without spaces" $ do
      runParser args "(2,3)" `shouldBe` [Lit (IntVal 2), Lit (IntVal 3)]
    
    it "operation as arg" $ do
      runParser args "( 2+3)" `shouldBe` [Add (Lit (IntVal 2)) (Lit (IntVal 3))]

    it "complex args expr" $ do
      runParser args "( a, \"b\", call(a,b), 1)" `shouldBe` [SymbolRef "a" , Lit (StringVal "b"), Call "call" [SymbolRef "a",SymbolRef "b"], Lit (IntVal 1)]

  -- describe "complex expressions"
  --   describe "addition" $ do
  --     it "simple addition" $ do
  --       runParser expr "2+3*v + call(arg1,1,\"str\")" `shouldBe` Add (Add (IntLit 2) (Mul (IntLit 3) (SymbolRef "v"))) (Call (SymbolRef "call") [SymbolRef "arg1", IntLit 1, StringLit "str"])