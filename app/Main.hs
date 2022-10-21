{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Monkey (interpret)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  src <- case args of
    [] -> TIO.getContents
    x : _ -> TIO.readFile x
  interpret src