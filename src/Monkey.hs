module Monkey
  ( interpret,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.State (runStateT)
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
    let scope = M.fromList [(pack "puts", Builtin Puts), (pack "len", Builtin Len)]
    void $ runStateT (executeStatements xs) [scope]