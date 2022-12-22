module Monkey
  ( interpret,
  )
where

import Control.Monad.Trans.State (runStateT)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Monkey.Eval (Builtin (Len, Puts), Value (Builtin), executeStatements)
import Monkey.Parser (program)
import System.Exit (die)
import Text.Megaparsec (errorBundlePretty, parse)
import Control.Monad.Trans.Except (runExceptT)

interpret :: Text -> IO ()
interpret src = case parse program "" src of
  Left bundle -> die $ errorBundlePretty bundle
  Right xs -> do
    print xs
    let scope = M.fromList [(pack "puts", Builtin Puts), (pack "len", Builtin Len)]
    result <- runStateT (runExceptT $ executeStatements xs) [scope]
    case result of
      (Left _, _) -> die "return cannot be used outside of a function"
      (Right _, _) -> pure ()