module Main where

import Text.Parsec
import HFortran.AST
import HFortran.Parser

import System.IO (readFile)
import System.Environment (getArgs)
import Data.List (isPrefixOf)

doParseAndShow :: String -> IO ()
doParseAndShow file = 
  do src <- readFile file
     case parse program file src of
       Right ast -> print ast
       Left x -> print x

main = do 
    files <- getArgs
    mapM_ doParseAndShow files

