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
import Monkey.Desugar
import System.Exit (die)
import Text.Megaparsec (errorBundlePretty, parse)
import Monkey.PrettyPrinter
import Prettyprinter
import System.IO (stdout)
import Prettyprinter.Render.Terminal (renderIO)

interpret :: Text -> IO ()
interpret src = case parse program "" src of
  Left bundle -> die $ errorBundlePretty bundle
  Right xs -> do
    pprStdout xs
    let desugared = desugarStatements xs
    pprStdout desugared
    let env = M.fromList [(pack "puts", Builtin Puts), (pack "len", Builtin Len)]
    env' <- traverse newIORef env
    result <- runStateT (runExceptT $ executeStatements desugared) [env']
    case result of
      (Left _, _) -> die "return cannot be used outside of a function"
      (Right _, _) -> pure ()


pprStdout :: Ppr a => a -> IO ()
pprStdout = renderIO stdout . reAnnotateS groupColor . layoutSmart defaultLayoutOptions . ppr