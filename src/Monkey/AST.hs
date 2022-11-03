module Monkey.AST where

import Data.Text (Text)

type Name = Text

type Params = [Name]

data Block = Block [Statement] (Maybe Expr) deriving (Show, Eq)

data Statement
  = Expr Expr
  | Let Name Expr
  | Assign Name Expr
  | Return (Maybe Expr)
  | While Expr Block
  deriving (Show, Eq)

data Expr
  = Var Name
  | LitInt Int
  | LitFLoat Double
  | LitBool Bool
  | LitString Text
  | LitArray [Expr]
  | Access Expr Name
  | Index Expr Expr
  | Call Expr [Expr]
  | If Expr Block (Maybe Block)
  | Fun Params Block
  | BinOp BinOp Expr Expr
  | Obj [(Expr, Expr)]
  deriving (Show, Eq)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | LessThan
  | GreaterThan
  deriving (Show, Eq)
