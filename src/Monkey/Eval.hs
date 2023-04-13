{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Monkey.Eval where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (traverse_)
import Data.HashMap.Strict as H
import Data.Hashable
import Data.IORef
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.Unique (Unique, newUnique)
import Data.Void (Void)
import Monkey.AST
import Text.Megaparsec (Parsec)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.List (intercalate)
import Control.Exception (assert)
import Data.Maybe (isNothing)

type Parser = Parsec Void Text

type Scope = M.Map Text (IORef Value)

type Interpreter = ExceptT Value (StateT [Scope] IO)

instance Hashable Value where
  hashWithSalt s = \case
    Null -> s `hashWithSalt` (0 :: Int)
    String t -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t
    Num n -> s `hashWithSalt` (2 :: Int) `hashWithSalt` n
    Bool b -> s `hashWithSalt` (3 :: Int) `hashWithSalt` b
    Float f -> s `hashWithSalt` (4 :: Int) `hashWithSalt` f
    Array u _ -> s `hashWithSalt` (5 :: Int) `hashWithSalt` u
    Object u _ -> s `hashWithSalt` (6 :: Int) `hashWithSalt` u
    Function u _ _ _ -> s `hashWithSalt` (7 :: Int) `hashWithSalt` u
    Builtin _ -> s `hashWithSalt` (8 :: Int) -- TODO: This will probably cause all builtins to be equal?

data Value
  = Null
  | String Text
  | Num Int
  | Bool Bool
  | Float Double
  | Builtin Builtin
  | Array Unique (MV.IOVector Value)
  | Object Unique (IORef (HashMap Value Value))
  | Function Unique Scope [Name] Block

instance Eq Value where
  String s == String s' = s == s'
  Num n == Num n' = n == n'
  Float f == Float f' = f == f'
  Bool b == Bool b' = b == b'
  Array u _ == Array u' _ = u == u'
  Object u _ == Object u' _ = u == u'
  Function u _ _ _ == Function u' _ _ _ = u == u'
  Null == Null = True
  _ == _ = False

data Builtin
  = Puts
  | Len

{- instance Show Value where
  show = \case
    Array _ x -> show x
    Bool x -> show x
    Builtin _ -> "<builtin>"
    Num x -> show x
    String x -> unpack x
    Object _ x ->
      let showPair (k, v) = show k ++ ": " ++ show v
          list = fmap showPair . H.toList . unsafePerformIO $ readIORef x
       in "{" ++ intercalate ", " list ++ "}"
    Function {} -> "<function>"
    Null -> "null"
    Float x -> show x -}

-- to allow division to work nicely
class Num a => Arith a where
  divide :: a -> a -> a

instance Arith Int where
  divide = div

instance Arith Double where
  divide = (/)

pprint :: Value -> IO String
pprint = \case
  Null -> pure "null"
  String x -> pure $ '"' : unpack x ++ "\""
  Num x -> pure $ show x
  Bool x -> pure $ show x
  Float x -> pure $ show x
  Builtin _ -> pure "<builtin>"
  Array _ xs -> do
    xs' <- V.toList <$> V.freeze xs
    list <- traverse pprint xs'
    pure $ "[" ++ intercalate ", " list ++ "]"
  Object _ x -> do
    x' <- readIORef x
    list <- traverse (\(k, v) -> (++) <$> pprint k <*> ((": " ++) <$> pprint v)) $ H.toList x'
    pure $ "{" ++ intercalate ", " list ++ "}"
  Function {} -> pure "<function>"


-- built in functions
puts :: [Value] -> Interpreter Value
puts xs = liftIO do
  strs <- traverse pprint xs
  traverse_ putStrLn strs
  pure Null

len :: [Value] -> Interpreter Value
--len (Array _ xs : _) = pure . Num . length $ xs
--len val = error $ "can't call len on " <> show val
len [Array _ xs] = pure $ Num $ MV.length xs
len _ = error "len takes a string, array, or object"

executeStatements :: [Statement] -> Interpreter ()
executeStatements = traverse_ execute

evalBlock :: Block -> Interpreter Value
evalBlock (Block statements x) =
  traverse_ execute statements *> case x of
    Just expr -> eval expr
    Nothing -> pure Null

execute :: Statement -> Interpreter ()
execute = \case
  Expr x -> void $ eval x
  Let n x -> do
    v <- eval x
    void $ createVar n v
  Assign lhs rhs op -> assert (isNothing op) do
    case lhs of
      Var n -> do
        v <- eval rhs
        setVar n v
      Index x ix -> eval x >>= \case
        Array _ xs -> eval ix >>= \case
          Num i -> do
            val <- eval rhs
            liftIO $ MV.write xs i val
          _ -> error "index must be a number"
        _ -> error "can't index non-array or non-object"
      Access x field -> eval x >>= \case
        Object _ ref -> do
          val <- eval rhs
          liftIO $ modifyIORef ref $ H.insert (String field) val
        _ -> error "can't access non-object"
      _ -> error "invalid assignment"
  While x b ->
    newScope M.empty $ do
      v <- eval x
      case v of
        Bool True -> evalBlock b *> execute (While x b)
        Bool False -> pure ()
        _ -> error "while condition is not a boolean"
  Block' b -> newScope M.empty $ void $ evalBlock b

createVar :: Name -> Value -> Interpreter ()
createVar n v = do
  (scope : scopes) <- lift get
  ref <- liftIO $ newIORef v
  let s = M.insert n ref scope
  lift $ put (s : scopes)

setVar :: Name -> Value -> Interpreter ()
setVar name value =
  lift get >>= mutate name value
  where
    mutate _ _ [] = error $ unpack name <> " is not defined"
    mutate n v (scope : scopes) =
      case M.lookup n scope of
        Just ref -> liftIO $ writeIORef ref value
        Nothing -> mutate n v scopes

newScope :: Scope -> Interpreter a -> Interpreter a
newScope scope action = do
  lift $ modify (scope :)
  action <* (lift . modify) tail

eval :: Expr -> Interpreter Value
eval = \case
  Var x -> getVar x
  LitInt x -> pure $ Num x
  LitFloat x -> pure $ Float x
  LitBool x -> pure $ Bool x
  LitString x -> pure $ String x
  LitArray x -> do
    u <- liftIO newUnique
    vals <- traverse eval x
    xs <- V.thaw vals
    pure $ Array u xs
  Access obj key ->
    eval obj >>= \case
      Object _ m -> do
        h <- liftIO $ readIORef m
        case H.lookup (String key) h of
          Just v -> pure v
          Nothing -> error $ "key " <> show key <> " not found in object"
      _ -> error "can't access key on non-object"
  Index obj key ->
    eval obj >>= \case
      Array _ xs ->
        eval key >>= \case
          Num i -> MV.read xs i
          _ -> error "index must be a number"
      Object _ hashmap -> do
        k <- eval key
        h <- liftIO $ readIORef hashmap
        case H.lookup k h of
          Just v -> pure v
          Nothing -> do
            k' <- liftIO $ pprint k
            error $ "key " <> k' <> " not found in object"
      -- use liftIO / pprint
      bad -> do
        bad' <- liftIO $ pprint bad
        error $ "can't index " <> bad' <> ", not an array or object"
  Call x args -> do
    values <- traverse eval args
    eval x >>= \case
      Builtin Puts -> puts values
      Builtin Len -> len values
      Function _ scope params body -> lift do
        vals <- traverse (liftIO . newIORef) values
        let env = scope <> M.fromList (zip params vals)
        s <- get
        put [env]
        res <- runExceptT (evalBlock body)
        put s
        pure (either id id res)
      badFunc -> do
        badFunc' <- liftIO $ pprint badFunc
        error $ "can't call " <> badFunc' <> ", not a function"
  If expr trueBlock falseBlock -> do
    result <- newScope M.empty (eval expr)
    case result of
      Bool True -> evalBlock trueBlock
      Bool False -> maybe (pure Null) evalBlock falseBlock
      _ -> error "not a boolean."
  Fun params block -> do
    u <- liftIO newUnique
    scopes <- lift get
    pure $ Function u (M.unions scopes) params block
  BinOp op x y -> binOp op <$> eval x <*> eval y
  UnOp op x -> unOp op <$> eval x
  Obj keypairs -> do
    u <- liftIO newUnique
    h <- H.fromList <$> traverse (bitraverse eval eval) keypairs
    ref <- liftIO $ newIORef h
    pure $ Object u ref
  Return m -> do
    v <- maybe (pure Null) eval m
    throwE v

unOp :: UnOp -> Value -> Value
unOp op x = case op of
  Not -> case x of
    Bool b -> Bool $ not b
    _ -> error "can't negate non-boolean"
  Negate -> case x of
    Num n -> Num $ negate n
    Float n -> Float $ negate n
    _ -> error "can't negate non-number"

binOp :: BinOp -> Value -> Value -> Value
binOp op l r = case op of
  Plus -> arithOp (+)
  Minus -> arithOp (-)
  Times -> arithOp (*)
  -- Exponent -> arithOp (**)
  Divide -> arithOp divide
  LessThan -> compOp (<)
  LessThanEqual -> compOp (<=)
  GreaterThan -> compOp (>)
  GreaterThanEqual -> compOp (>=)
  Equal -> compOp (==)
  NotEqual -> compOp (/=)
  And -> case (l, r) of
    (Bool x, Bool y) -> Bool $ x && y
    _ -> error "can't and non-booleans"
  Or -> case (l, r) of
    (Bool x, Bool y) -> Bool $ x || y
    _ -> error "can't or non-booleans"
  where
    arithOp :: (forall a. Arith a => a -> a -> a) -> Value
    arithOp f = case (l, r) of
      (Num x, Num y) -> Num $ f x y
      (Float x, Float y) -> Float $ f x y
      (Num x, Float y) -> Float $ f (fromIntegral x) y
      (Float x, Num y) -> Float $ f x (fromIntegral y)
      _ -> error $ show op <> " is not supported for the supplied values"
    compOp :: (forall a. Ord a => a -> a -> Bool) -> Value
    compOp f = case (l, r) of
      (Num x, Num y) -> Bool $ f x y
      (Float x, Float y) -> Bool $ f x y
      (Num x, Float y) -> Bool $ f (fromIntegral x) y
      (Float x, Num y) -> Bool $ f x (fromIntegral y)
      _ -> error $ show op <> " is not supported for the supplied values"
        -- error $ show op <> " is not supported for " <> show l <> " and " <> show r

getVar :: Text -> Interpreter Value
getVar n = do
  env <- lift get
  liftIO $ find n env
  where
    find name [] = error $ unpack name <> " is not defined"
    find name (x : xs) = case M.lookup name x of
      Nothing -> find name xs
      Just ref -> readIORef ref