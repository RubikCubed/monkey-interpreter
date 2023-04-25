{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Monkey.Desugar where

import Monkey.AST
import Data.Bifunctor (bimap)

desugarStatements :: [Statement] -> [Statement]
desugarStatements = fmap $ \case  -- Assign x (BinOp op x y) Nothing
  Assign x y (Just op) -> Block' $ Block [Let "tmp" x, Assign x (BinOp op (Var "tmp") y) Nothing] Nothing
  Assign x y Nothing -> Assign x y Nothing
  While x b -> While x $ desugarBlock b
  Block' b -> Block' $ desugarBlock b
  Expr x -> Expr $ desugarExpression x
  Let n x -> Let n $ desugarExpression x

desugarBlock :: Block -> Block
desugarBlock (Block stmts ret) = Block (desugarStatements stmts) ret

desugarExpression :: Expr -> Expr
desugarExpression = \case
  Fun params b -> Fun params $ desugarBlock b
  Call x args -> Call (desugarExpression x) (fmap desugarExpression args)
  If x b1 b2 -> If (desugarExpression x) (desugarBlock b1) (fmap desugarBlock b2)
  BinOp op x y -> BinOp op (desugarExpression x) (desugarExpression y)
  UnOp op x -> case op of
    Increment -> BinOp Plus x (LitInt 1)
    Decrement -> BinOp Minus x (LitInt 1)
    _ -> UnOp op $ desugarExpression x
  Obj xs -> Obj $ fmap (bimap desugarExpression desugarExpression) xs
  Return x -> Return $ fmap desugarExpression x
  x -> x
