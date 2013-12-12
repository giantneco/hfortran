module HFortran.AST where

import Text.Parsec
import Text.Parsec.String

-- typedef Identifier = String

-- TODO
-- data Kind = ?

data FortranConstant =
    CharLiteralConstant String
  | IntLiteralConstant Int (Maybe(FortranConstant))
  -- | IntLiteralConstantKinded Int (Maybe(FortranConstant))
  deriving (Show, Eq)

data FortranBaseType =
    FInteger
  | FReal
  | FDoublePrecision
  | FComplex
  | FCharacter
  | FLogical
  | FType
  deriving (Show, Eq)

data UnaryOp =
    Not
  | UnaryAdd
  | UnarySub
  | DefinedUnaryOp String
  deriving (Show, Eq)

data BinaryOp =
    And
  | Or
  | Equiv
  | NEquiv
  | DefinedBinaryOp String
  | Equal
  | NEqual
  | LessT
  | LessE
  | GreT    
  | GreE
  | Concat
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show, Eq)

data Expression =
    Constant FortranConstant
  | Variable String
  | UnaryOperand UnaryOp Expression
  | BinaryOperand BinaryOp Expression Expression
  deriving (Show, Eq)

data FortranDeclaration =
  TypeDeclaration FortranBaseType [String]
  deriving (Show, Eq)

data FortranFormat =
  FormatAsterisc
  deriving (Show, Eq)

data FortranExecute =
  Assignment Expression Expression
  | Print FortranFormat [FortranConstant]  -- R911
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
