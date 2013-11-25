module Main where

import Text.Parsec hiding (spaces)
import Text.Parsec.String

data FortranTopLevel 
  = Program
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving Show

fortranCode :: Parser [FortranTopLevel]
fortranCode = undefined

fortranTopLevel :: Parser FortranTopLevel
fortranTopLevel = undefined

-- program :: Parser FortranTopLevel

spaces :: Parser ()
spaces = skipMany1 space

programStatement :: Parser String
programStatement = do
  string "program"
  spaces
  return "hoge"

program :: Parser FortranTopLevel
program = do
  name <- programStatement
  string "end"
  spaces
  string "program"
  return Program
