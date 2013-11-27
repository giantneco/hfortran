module Main where

import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

data FortranDeclaration
     = TypeDeclaration
     deriving Show

data FortranExecute
     = Assignment
     | Call
     | Continue
     deriving Show

data FortranTopLevel
  = Program String
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving Show

fortranCode :: Parser [FortranTopLevel]
fortranCode = undefined

fortranTopLevel :: Parser FortranTopLevel
fortranTopLevel = undefined

-- program :: Parser FortranTopLevel

space :: Parser ()
space = do
  oneOf " \t"
  return ()

spaces :: Parser ()
spaces = do
  skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space

eol :: Parser ()
eol = do
  char '\n'
  return ()

identifier :: Parser String
identifier = do
  first <- letter
  rest <- many (letter <|> digit)
  return (first : rest)

programStatement :: Parser String
programStatement = do
  spaces
  string "program"
  spaces
  programName <- identifier
  spaces
  eol
  return $ programName

endProgramStatement :: String -> Parser ()
endProgramStatement programName = do
  spaces
  string "end"
  spaces
  string "program"
  spaces
  notFollowedBy alphaNum <|> do {string programName; return ()} <?> "end program " ++ programName
  eol
  return ()

program :: Parser FortranTopLevel
program = do
  programName <- programStatement
  endProgramStatement programName
  eof
  return $ Program programName

main = putStrLn "not yet"
