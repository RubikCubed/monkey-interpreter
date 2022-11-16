{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Monkey.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Text (Text, cons, pack)
import Data.Void
import Monkey.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

expression :: Parser Expr
expression =
  makeExprParser
    form
    [ 
      [Prefix (UnOp Not <$ symbol "!"), Prefix (UnOp Negate <$ symbol "-")],
      [InfixL (BinOp Times <$ symbol "*"), InfixL (BinOp Divide <$ symbol "/")],
      [InfixL (BinOp Plus <$ symbol "+"), InfixL (BinOp Minus <$ symbol "-")],
      [InfixL (BinOp LessThan <$ symbol "<"), InfixL (BinOp LessThanEqual <$ symbol "<="), InfixL (BinOp GreaterThan <$ symbol ">"), InfixL (BinOp GreaterThanEqual <$ symbol ">=")],
      [InfixL (BinOp Equal <$ symbol "=="), InfixL (BinOp NotEqual <$ symbol "!=")],
      [InfixL (BinOp And <$ symbol "&&")],
      [InfixL (BinOp Or <$ symbol "||")]
    ]

form :: Parser Expr
form = do
  x <- atom
  xs <- many (call <|> index <|> access)
  pure (Data.Foldable.foldl' (&) x xs)
  where
    call = do
      args <- parens (expression `sepBy` symbol ",") <?> "argument list"
      pure \f -> Call f args
    index = do
      i <- between (symbol "[") (symbol "]") expression <?> "index"
      pure \x -> Index x i
    access = do
      _ <- symbol "." <?> "member access"
      n <- name
      pure \x -> Access x n

atom :: Parser Expr
atom =
  choice
    [ parens expression <?> "parenthesized expression",
      arr,
      obj,
      function,
      string_,
      bool,
      if_,
      try (float <?> "double"),
      int <?> "integer",
      var <?> "variable"
      -- , Bool <$> bool <?> "boolean"
    ]

statement :: Parser Statement
statement =
  choice
    [ let_ <* semicolon,
      try (assign <* semicolon),
      return_ <* semicolon,
      while,
      try (Expr <$> expression <* semicolon)
    ]

program :: Parser [Statement]
program = hidden sc *> many statement

semicolon :: Parser ()
semicolon = skipSome $ symbol ";"

obj :: Parser Expr
obj = do
  _ <- symbol "{"
  props <- commaSep prop
  _ <- symbol "}"
  pure $ Obj $ props

prop :: Parser (Expr, Expr)
prop = do
  key <- expression
  _ <- symbol ":"
  value <- expression
  pure (key, value)

assign :: Parser Statement
assign = do
  name' <- name
  _ <- symbol "="
  expr <- expression
  pure $ Assign name' expr


let_ :: Parser Statement
let_ = do
  _ <- symbol "let"
  name' <- name
  _ <- symbol "="
  expr <- expression
  pure $ Let name' expr

while :: Parser Statement
while = do
  _ <- symbol "while"
  expr <- parens expression
  block' <- block
  pure $ While expr block'


--  Block [Statement] (Maybe Expr)
if_ :: Parser Expr
if_ = do
  _ <- symbol "if"
  cond <- parens expression
  true <- block
  false <- optional $ symbol "else" *> block
  pure $ If cond true false

block :: Parser Block
block = squiggles $ do
  stmts <- many statement
  ret <- optional expression
  pure $ Block stmts ret

return_ :: Parser Statement
return_ = do
  _ <- symbol "return"
  expr <- optional expression
  pure $ Return expr

function :: Parser Expr
function = do
  _ <- symbol "fn"
  args <- parens $ commaSep name
  body <- block
  pure $ Fun args body

functionCall :: Parser Expr
functionCall = do
  fn <- var <|> parens expression
  args <- parens $ commaSep expression
  pure $ Call fn args

bool :: Parser Expr
bool = LitBool True <$ symbol "true" <|> LitBool False <$ symbol "false"

var :: Parser Expr
var = Var <$> name

string_ :: Parser Expr
string_ = do
  s <- lexeme $ char '"' *> manyTill L.charLiteral (char '"')
  pure . LitString . pack $ s

int :: Parser Expr
int = LitInt <$> lexeme L.decimal

float :: Parser Expr
float = LitFLoat <$> lexeme L.float

arr :: Parser Expr
arr = LitArray <$> brackets (commaSep expression)

-- helper functions
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squiggles :: Parser a -> Parser a
squiggles = between (symbol "{") (symbol "}")

name :: Parser Name
name = lexeme $ do
  c <- letterChar
  cs <- many alphaNumChar
  pure $ c `cons` (pack cs)

commaSep :: Parser a -> Parser [a]
commaSep = (`sepBy` symbol ",")

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc