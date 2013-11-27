module HFortran.AST where

import Text.Parsec
import Text.Parsec.String

data FortranDeclaration
     = TypeDeclaration
     deriving (Show, Eq)

data FortranExecute
     = Assignment
     | Call
     | Continue
     deriving (Show, Eq)

data FortranTopLevel
  = Program String [FortranDeclaration] [FortranExecute]
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving (Show, Eq)
