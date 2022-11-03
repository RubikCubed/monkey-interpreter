{-# LANGUAGE LambdaCase #-}
module Monkey
  ( interpret,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.State (runStateT)
import qualified Data.Map as M
import Data.Text (Text)
import Monkey.Eval (Builtin (Len, Puts), Value (Builtin), executeStatements)
import Monkey.Parser (program)
import System.Exit (die)
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Text (pack)

interpret :: Text -> IO ()
interpret src = case parse program "" src of
  Left bundle -> die $ errorBundlePretty bundle
  Right xs -> do
    -- print xs
    let scope = M.fromList [(pack "puts", Builtin Puts), (pack "len", Builtin Len)]
    void $ runStateT (executeStatements xs) [scope]