module Main where

import Text.Parsec
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
