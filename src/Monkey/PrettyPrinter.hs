{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Monkey.PrettyPrinter where

import Monkey.AST
    ( BinOp(..), Block(..), Expr(..), Statement(..), UnOp(..) )

--import Prettyprinter (Doc, pretty, (<+>))
--import Prettyprinter qualified as PP

import Prettyprinter
import Data.Foldable (toList)
import Prettyprinter.Render.Terminal

class Ppr a where
  ppr :: a -> Doc Highlight

instance Ppr [Statement] where
  ppr :: [Statement] -> Doc Highlight
  ppr = vsep . fmap ppr

instance Ppr Statement where
  ppr :: Statement -> Doc Highlight
  ppr = \case
    Expr x -> ppr x <> semi
    Let n x -> kw "let" <+> pretty n <+> "=" <+> ppr x <> semi
    Assign lhs rhs op -> ppr lhs <+> foldMap ppr op <> "=" <+> ppr rhs <> semi
    While x b -> kw "while" <+> ppr x <+> ppr b

instance Ppr Expr where
  ppr :: Expr -> Doc Highlight
  ppr = \case
    Var n -> pretty n
    LitInt x -> num $ pretty x
    LitFloat x -> num $ pretty x
    LitBool x -> num $ pretty x
    LitString x -> str $ pretty . show $ x
    LitArray xs -> "[" <+> hsep (punctuate comma (ppr <$> toList xs)) <+> "]"
    If cond true false -> kw "if" <+> parens (ppr cond) <+> ppr true <> false'
      where
        false' = case false of
          Nothing -> ""
          Just block -> kw " else" <+> ppr block
    Fun params body -> fn "fn" <> parens (hsep (punctuate comma (pretty <$> params))) <+> ppr body
    BinOp op x y -> ppr x <+> ppr op <+> ppr y
    UnOp op x -> ppr op <> ppr x
    Call x args -> fn (ppr x) <> parens (hsep (punctuate comma (ppr <$> args)))
    Obj fields -> nest 2 ("{" <> line <> vsep (punctuate comma (field <$> fields))) <> line <> "}"
      where
        field (key, val) = ppr key <> colon <+> ppr val
    Return x -> kw "return" <+> foldMap ppr x
    Access x y -> ppr x <> "." <> pretty y
    Index x y -> ppr x <> "[" <> ppr y <> "]"
    Block' b -> ppr b


instance Ppr Block where
  ppr :: Block -> Doc Highlight
  ppr (Block ss ret) = nest 2 ("{" <> line <> body) <> line <> "}"
    where
      ret' = case ret of
        Nothing -> []
        Just x -> [ppr x <> semi]
      body = vsep ((ppr <$> ss) ++ ret')

instance Ppr BinOp where
  ppr :: BinOp -> Doc Highlight
  ppr = \case
    Times -> "*"
    Divide -> "/"
    Plus -> "+"
    Minus -> "-"
    LessThan -> "<"
    LessThanEqual -> "<="
    GreaterThan -> ">"
    GreaterThanEqual -> ">="
    Equal -> "=="
    NotEqual -> "!="
    And -> "&&"
    Or -> "||"

instance Ppr UnOp where
  ppr :: UnOp -> Doc Highlight
  ppr = \case
    Not -> "!"
    Negate -> "-"
    PreIncrement -> "++"
    PreDecrement -> "--"
    PostIncrement -> "++"
    PostDecrement -> "--"

data Highlight
  = Keyword
  | String
  | Number
  | Function

kw :: Doc Highlight -> Doc Highlight
kw = annotate Keyword

str :: Doc Highlight -> Doc Highlight
str = annotate String

num :: Doc Highlight -> Doc Highlight
num = annotate Number

fn :: Doc Highlight -> Doc Highlight
fn = annotate Function

groupColor :: Highlight -> AnsiStyle
groupColor = \case
  Keyword -> colorDull Red
  String -> color Cyan
  Number -> color Blue
  Function -> color Green
