module Monkey
  ( interpret,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import Data.IORef (newIORef)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Monkey.Eval (Builtin (Len, Puts), Value (Builtin), executeStatements)
import Monkey.Parser (program)
import System.Exit (die)
import Text.Megaparsec (errorBundlePretty, parse)

interpret :: Text -> IO ()
interpret src = case parse program "" src of
  Left bundle -> die $ errorBundlePretty bundle
  Right xs -> do
    print xs
    let env = M.fromList [(pack "puts", Builtin Puts), (pack "len", Builtin Len)]
    env' <- traverse newIORef env
    result <- runStateT (runExceptT $ executeStatements xs) [env']
    case result of
      (Left _, _) -> die "return cannot be used outside of a function"
      (Right _, _) -> pure ()