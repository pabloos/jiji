
module Parser.AST where

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Negate Expr
          | IntLit Int
          | BoolLit Bool
          | StringLit String
          | SymbolRef String
          | Call String [Expr]
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Eq Expr Expr
          | Neq Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Le Expr Expr
          | Ge Expr Expr
          deriving (Show, Eq)

data Statement = If Expr [Statement]
               | IfElse Expr [Statement] [Statement]
               | While Expr [Statement]
               | Assign String Expr
               | Print Expr
               | Return Expr
               | Empty
               deriving (Show, Eq)

data Definition = Constant String Expr
                | Var String Expr
                | Fun String [Expr] [Statement]
                | NoDef
                deriving (Show, Eq)

data Main = Main Statement deriving (Show, Eq)

data Program = Program [Definition] Main deriving (Show, Eq)
