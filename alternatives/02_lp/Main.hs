module Main where

import TikzOutput
import System.Environment

main = do
  a <- getArgs
  let fn = a !! 0
  test fn ""
