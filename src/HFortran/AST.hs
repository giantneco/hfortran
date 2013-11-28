module HFortran.AST where

import Text.Parsec
import Text.Parsec.String

-- typedef Identifier = String

data FortranBaseType =
  FInteger
  deriving (Show, Eq)

data FortranDeclaration
     = TypeDeclaration FortranBaseType String
     deriving (Show, Eq)

data FortranExecute
     = Assignment
     | Call
     | Continue
     deriving (Show, Eq)

-- data FortranStatement
--   = ProgramStatement String
--   | EndProgramStatement Maybe(String)

data FortranTopLevel
  = Program String [FortranDeclaration] [FortranExecute]
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving (Show, Eq)
