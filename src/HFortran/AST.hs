module HFortran.AST where

-- typedef Identifier = String

-- TODO
-- data Kind = ?

data Symbol = Symbol String deriving (Show, Eq)

symbolContent :: Symbol -> String
symbolContent (Symbol string) = string

readSymbol :: Read a => Symbol -> a
readSymbol (Symbol string) = read string

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
  | DefinedUnaryOp Symbol
  deriving (Show, Eq)

data BinaryOp =
    And
  | Or
  | Equiv
  | NEquiv
  | DefinedBinaryOp Symbol
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
  | Variable Symbol
  | FunctionReference Symbol [Expression]
  | UnaryOperand UnaryOp Expression
  | BinaryOperand BinaryOp Expression Expression
  deriving (Show, Eq)

-- data ImplicitSpec =

data TypeAttr = 
  Parameter
  | Allocatable
  | Dimension [ArraySpec]
  | External
  | Intent IntentSpec
  | Intrinsic
  | Optional
  | Pointer
  | Save
  | Target
  deriving (Show, Eq)

data ArraySpec =
  ExplicitArraySpec (Maybe(Expression)) Expression
  | AssumedArraySpec (Maybe(Expression))
  | AssumedSize (Maybe(Expression))
  | DeferredArraySpec
  deriving (Show, Eq)

data IntentSpec = In | Out | InOut deriving (Show, Eq)

data AccessAttr =
  Public
  | Private
  deriving (Show, Eq)

data Attr = Type TypeAttr | Access AccessAttr deriving (Show, Eq)

data FortranDeclaration =
  TypeDeclaration FortranBaseType (Maybe([Attr])) [Symbol]
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
--   = ProgramStatement Symbol
--   | EndProgramStatement Maybe(Symbol)

data FortranTopLevel
  = Program Symbol [FortranDeclaration] [FortranExecute]
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving (Show, Eq)
