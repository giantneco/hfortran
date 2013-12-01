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
eol = do {char '\n'; return ()} <|> eof

identifier :: Parser String
identifier = do
  first <- letter
  rest <- many (letter <|> digit)
  return (first : rest)

-- | R502 type-spec
typeSpec :: Parser FortranBaseType
typeSpec = (do { string "integer"; return FInteger })
       <|> (do { string "real"; return FReal })
       <|> (do { string "double"; spaces; string "precision"; return FDoublePrecision})
       <|> (do { string "complex"; return FComplex })
       <|> (do { string "character"; return FCharacter})
       <|> (do { string "logical"; return FLogical })
       <|> (do { string "type"; spaces ; char '(' ; spaces ; identifier; spaces; char ')'; return FType })

commaSep :: Parser ()
commaSep = try $ do
           spaces
           char ','
           spaces

typeDeclarationStatement :: Parser FortranDeclaration
typeDeclarationStatement = do
  spaces
  baseType <- typeSpec
  spaces
  ids <- sepBy identifier commaSep
  spaces
  eol
  return $ TypeDeclaration baseType ids

declarationStatement :: Parser FortranDeclaration
declarationStatement = do try (typeDeclarationStatement)

continueStatement :: Parser FortranExecute
continueStatement = do
  spaces
  string "continue"
  spaces
  eol
  return $ Continue

executeStatement :: Parser FortranExecute
executeStatement = do try (continueStatement)

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
  spaces
  eol
  return ()

program :: Parser FortranTopLevel
program = do
  programName <- programStatement
  decls <- many declarationStatement
  cs <- many executeStatement
  endProgramStatement programName
  return $ Program programName decls cs

testParser :: Parser [String]
testParser = do
  a <- many $ try ( do {spaces; ret <- string "a"; return ret} )
  b <- many $ do {spaces; ret <- string "b"; return ret}
  eof
  return $ a ++ b