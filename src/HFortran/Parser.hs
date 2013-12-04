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

line :: Parser a -> Parser a
line psr = do
  spaces
  ret <- psr
  spaces
  eol
  return ret

-- | comma separater
commaSep :: Parser ()
commaSep = try $ do
           spaces
           char ','
           spaces

-- | R420 char-literal-constant
charLiteralConstant :: Parser FortranConstant
charLiteralConstant = (do { char '"'; str <- many (noneOf "\""); char '"'; return $ CharLiteralConstant str })
                  <|> (do { char '\''; str <- many (noneOf "\'"); char '\''; return $ CharLiteralConstant str })

-- | R502 type-spec
typeSpec :: Parser FortranBaseType
typeSpec = (do { string "integer"; return FInteger })
       <|> (do { string "real"; return FReal })
       <|> (do { string "double"; spaces; string "precision"; return FDoublePrecision})
       <|> (do { string "complex"; return FComplex })
       <|> (do { string "character"; return FCharacter})
       <|> (do { string "logical"; return FLogical })
       <|> (do { string "type"; spaces ; char '(' ; spaces ; identifier; spaces; char ')'; return FType })

typeDeclarationStatement :: Parser FortranDeclaration
typeDeclarationStatement = line $ do
  baseType <- typeSpec
  spaces
  ids <- sepBy identifier commaSep
  return $ TypeDeclaration baseType ids

declarationStatement :: Parser FortranDeclaration
declarationStatement = do try (typeDeclarationStatement)

-- | R911 print-stmt (WIP)
printStatement :: Parser FortranExecute
printStatement = line $  do
  string "print"
  spaces
  fmt <- format
  spaces
  outputItemList <- many $ try $ do { commaSep ; item <- charLiteralConstant; return item }
  return $ Print fmt outputItemList

-- | R913 format
format :: Parser FortranFormat
format = (do char '*'; return FormatAsterisc)
     -- <|> (do ?; return FormatDefaultCharExpr) -- TODO
     -- <|> (do ?; return Lable)                 -- TODO

-- -- | R910 write-stmt (WIP)
-- writeStatement :: Parser FortranExecute
-- writeStatement = do
--   spaces
--   string "write"
--   spaces
--   char '('
--   spaces
--   ioControlSpecList
--   spaces
--   char ')'
  
-- -- | 912 io-control-spec-list
-- ioControlSpecList :: Parser 

continueStatement :: Parser FortranExecute
continueStatement = line $ do
  string "continue"
  return $ Continue

executeStatement :: Parser FortranExecute
executeStatement = try continueStatement
               <|> try printStatement

programStatement :: Parser String
programStatement = line $ do
  string "program"
  spaces
  programName <- identifier
  return $ programName

endProgramStatement :: String -> Parser ()
endProgramStatement programName = line $ do
  string "end"
  spaces
  string "program"
  spaces
  notFollowedBy alphaNum <|> do {string programName; return ()} <?> "require end program " ++ programName
  return ()

program :: Parser FortranTopLevel
program = do
  programName <- programStatement
  decls <- many declarationStatement
  exes <- many executeStatement
  endProgramStatement programName
  return $ Program programName decls exes

testParser :: Parser [String]
testParser = do
  a <- many $ try ( do {spaces; ret <- string "a"; return ret} )
  b <- many $ do {spaces; ret <- string "b"; return ret}
  eof
  return $ a ++ b