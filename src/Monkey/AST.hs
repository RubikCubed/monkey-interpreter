module Monkey.AST where

import Data.Text (Text)
import Data.Vector (Vector)

type Name = Text

type Params = [Name]

data Block = Block [Statement] (Maybe Expr) deriving (Show, Eq)

data Statement
  = Expr Expr
  | Let Name Expr
  | Assign Expr Expr
  | While Expr Block
  deriving (Show, Eq)

data Expr
  = Var Name
  | LitInt Int
  | LitFLoat Double
  | LitBool Bool
  | LitString Text
  | LitArray (Vector Expr)
  | Access Expr Name
  | Index Expr Expr
  | Call Expr [Expr]
  | If Expr Block (Maybe Block)
  | Fun Params Block
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | Obj [(Expr, Expr)]
  | Return (Maybe Expr)
  deriving (Show, Eq)

data UnOp
  = Not
  | Negate
  deriving (Show, Eq)

data BinOp
  = Times
  | Divide
  | Plus
  | Minus
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | Equal
  | NotEqual
  | And
  | Or
  deriving (Show, Eq)
