module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

data FortranTopLevel
  = Program (Maybe String)
  -- | Subroutine
  -- | Function
  -- | Module     -- TODO
    deriving Show

fortranCode :: Parser [FortranTopLevel]
fortranCode = undefined

fortranTopLevel :: Parser FortranTopLevel
fortranTopLevel = undefined

-- program :: Parser FortranTopLevel

spaces1 :: Parser ()
spaces1 = skipMany1 space

identifier :: Parser String
identifier = do
  first <- letter
  rest <- many (letter <|> digit)
  return (first : rest)

programStatement :: Parser (Maybe String)
programStatement = do
  spaces
  string "program"
  spaces
  programName <- optionMaybe identifier
  return $ programName

endProgramStatement :: Parser (Maybe String)
endProgramStatement = do
  spaces
  string "end"
  spaces
  string "program"
  spaces
  programName <- optionMaybe identifier
  return $ programName

program :: Parser FortranTopLevel
program = do
  name1 <- programStatement
  name2 <- endProgramStatement
  if Just(False) == ((==) <$> name1 <*> name2)
    then error "not match"
    else return $ Program name1

main = putStrLn "not yet"
