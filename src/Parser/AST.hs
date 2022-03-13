
module Parser.AST where

data Literal = IntVal Int | BoolVal Bool | StringVal String
    deriving (Eq, Show)

data TypeValue = IntType | BoolType | StringType
            deriving (Eq, Show)

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Negate Expr
          | Lit Literal
          | SymbolRef String
          | Call String [Expr]
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Eq Expr Expr
          | Ne Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Le Expr Expr
          | Ge Expr Expr
          deriving (Show, Eq)

data Statement = If Expr [Statement]
               | IfElse Expr [Statement] [Statement]
               | While Expr [Statement]
               | Var String TypeValue Expr
               | Print Expr
               | Return Expr
               deriving (Show, Eq)

data Definition = Constant String Literal
                | Subprogram String [Param] (Maybe TypeValue) [Statement]
                deriving (Show, Eq)

type Param = (String, TypeValue)

type AST = [Definition]