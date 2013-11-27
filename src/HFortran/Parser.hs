module HFortran.Parser where

import HFortran.AST

import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

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

continueStatement :: Parser FortranExecute
continueStatement = do
  spaces
  string "continue"
  spaces
  eol
  return $ Continue

executeStatement :: Parser FortranExecute
executeStatement = continueStatement

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
  cs <- many continueStatement
  endProgramStatement programName
  eof
  return $ Program programName [] cs
