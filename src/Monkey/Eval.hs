{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- temporary
{-# OPTIONS_GHC -Wno-unused-matches #-}
--

module Monkey.Eval where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.State
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Void (Void)
import Monkey.AST
import Text.Megaparsec (Parsec)
import Control.Monad (void)

type Parser = Parsec Void Text

type Scope = M.Map String Value

type Interpreter = StateT [Scope] IO

data Value
  = String Text
  | Num Int
  | Bool Bool
  | Float Double
  | Array [Value]
  | Builtin Builtin
  | Function [Value] [Value] -- fix later
  | Null

data Builtin
  = Puts
  | Len

instance Show Value where
  show = \case
    Array x -> show x
    Bool x -> show x
    Builtin _ -> "<builtin>"
    Num x -> show x
    String x -> unpack x

-- built in functions
puts :: [Value] -> Interpreter Value
puts xs = liftIO (traverse_ print xs) $> Null

len :: [Value] -> Interpreter Value
len (Array xs : _) = pure . Num . length $ xs
len val = error $ "can't call len on " <> show val

executeStatements :: [Statement] -> Interpreter ()
executeStatements = traverse_ execute

evalBlock :: Block -> Interpreter Value
evalBlock (Block statements x) =
  traverse_ execute statements *> case x of
    Just expr -> eval expr
    Nothing   -> pure Null

execute :: Statement -> Interpreter ()
execute = \case
  Expr x -> void $ eval x
  Let n x -> do
    v <- eval x
    void $ createVar n v
  Assign n x -> do
    v <- eval x
    void $ setVar n v
  While x b -> do
    v <- eval x
    case v of
      Bool True -> evalBlock b *> execute (While x b) -- todo: is this right?
      Bool False -> pure ()
      _ -> error "while condition is not a boolean"
  Return m -> do
    v <- maybe (pure Null) eval m
    void $ pure v -- todo: does this even make sense? -- Arc: Your Interpreter monad is not yet equipped to handle early returns.

createVar :: Name -> Value -> Interpreter ()
createVar n v = do
  (scope:scopes) <- get
  let s = M.insert n v scope
  put (s : scopes)

setVar :: Name -> Value -> Interpreter ()
setVar name value = do
  modify $ mutate name value
 where
  mutate _ _ [] = error $ name <> " is not defined"
  mutate n v (scope:scopes) = if M.member n scope
    then M.insert n v scope : scopes
    else scope : mutate n v scopes
{- 
setVar n = modify . map $ M.insert n
  where
    find name [] = error $ name <> " is not defined"
    find name (x : xs) = case M.lookup name x of
      Nothing -> find name xs
      Just v -> v -}


eval :: Expr -> Interpreter Value
eval = \case
  Var x -> getVar x
  LitInt x -> pure $ Num x
  LitFLoat x -> pure $ Float x
  LitBool x -> pure $ Bool x
  LitString x -> pure $ String x
  LitArray exprs -> Array <$> traverse eval exprs
  -- Access Expr Name ->
  -- Index Expr Expr ->
  Call x args -> do
    values <- traverse eval args
    eval x >>= \case
      Builtin Puts -> puts values
      Builtin Len -> len values
      badValue -> error $ show badValue <> " is not a valid function"
  If expr trueBlock falseBlock -> eval expr >>= \case
    Bool True  -> evalBlock trueBlock
    Bool False -> maybe (pure Null) evalBlock falseBlock
    _          -> error "not a boolean."
-- Fun Params Block ->
  BinOp op x y -> evalInfix op <$> eval x <*> eval y
-- Obj [(Expr, Expr)] ->
  n -> error $ show n <> " is not supported yet"


evalInfix :: BinOp -> Value -> Value -> Value
evalInfix Plus (Num x) (Num y) = Num (x + y)
evalInfix Plus (Float x) (Float y) = Float (x + y)
evalInfix Times (Num x) (Num y) = Num (x * y)
evalInfix Times (Float x) (Float y) = Float (x * y)
evalInfix Minus (Num x) (Num y) = Num (x - y)
evalInfix Minus (Float x) (Float y) = Float (x - y)
evalInfix Divide (Num x) (Num y) = Num (x `div` y)
evalInfix Divide (Float x) (Float y) = Float (x / y)
evalInfix LessThan (Num x) (Num y) = Bool (x < y)
evalInfix LessThan (Float x) (Float y) = Bool (x < y)
evalInfix GreaterThan (Num x) (Num y) = Bool (x > y)
evalInfix GreaterThan (Float x) (Float y) = Bool (x > y)
evalInfix op x y = error $ show op <> " is not supported for " <> show x <> " and " <> show y


getVar :: String -> Interpreter Value
getVar n = fmap (find n) get
  where
    find name [] = error $ name <> " is not defined"
    find name (x : xs) = case M.lookup name x of
      Nothing -> find name xs
      Just v -> v