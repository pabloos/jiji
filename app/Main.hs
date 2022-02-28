module Main where

import Lib

import Parser.Type
import Control.Monad

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a