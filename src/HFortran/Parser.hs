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

-- | R304 name
-- TODO length check
name :: Parser String
name = do
  first <- letter
  rest <- many $ letter <|> digit <|> (char '_')
  -- if length (first : rest) > 31 then  ?
  return $ first : rest

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

-- | R401 signed-digit-string
signedDigitString :: Parser String
signedDigitString = do
  sign <- optionMaybe sign
  digits <- digitString
  case sign of
    Nothing -> return digits
    Just x -> return $ x : digits

-- | R402 digit-string
digitString :: Parser String
digitString = many1 digit

-- | R403 signed-int-literal-constant
signedIntLiteralConstant :: Parser FortranConstant
signedIntLiteralConstant = do
  sign <- optionMaybe sign
  digit <- digitString
  kind <- optionMaybe $ do { char '_'; kindParam }
  let value = read digit :: Int
    in
    case sign of
      Nothing -> return $ IntLiteralConstant value kind
      Just x -> return $ IntLiteralConstant (value * (if x == '+' then 1 else -1)) kind

-- | R404 int-literal-constant
intLiteralConstant :: Parser FortranConstant
intLiteralConstant = do
  digit <- digitString
  kind <- optionMaybe $ do { char '_'; kindParam }
  return $ IntLiteralConstant (read digit) kind

-- | R405 kind-param
-- TODO accept named constant
kindParam :: Parser FortranConstant
kindParam = do
  digit <- digitString
  return $ IntLiteralConstant (read digit) Nothing

-- | R406 sign
sign :: Parser Char
sign = char '+' <|> char '-'

-- | R420 char-literal-constant
charLiteralConstant :: Parser FortranConstant
charLiteralConstant = (do { char '"'; str <- many (noneOf "\""); char '"'; return $ CharLiteralConstant str })
                  <|> (do { char '\''; str <- many (noneOf "\'"); char '\''; return $ CharLiteralConstant str })

constant :: Parser FortranConstant
constant = signedIntLiteralConstant <|> charLiteralConstant

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

-- R601 variable
variable :: Parser Expression
variable = (do { name <- name; return $ Variable name }) -- scalar-variable-name or array-variable-name

declarationStatement :: Parser FortranDeclaration
declarationStatement = do try (typeDeclarationStatement)

-- | R701 Primary
primary :: Parser Expression
primary = try (do { constant <- constant; return $ Constant constant})
--       <|> constantSubobject
      <|> variable
--       <|> arrayConstructor
--       <|> structConstructor
--       <|> functionReference
      <|> (do {char '('; spaces; ret <- expression; spaces; char ')'; return ret})

-- R702 constant subobject

-- | R703
level1Expr :: Parser Expression
level1Expr = do
  first <- optionMaybe(definedUnaryOp)
  rest <- primary
  case first of
    Nothing -> return rest
    Just (duop) -> return $ duop rest

-- | R704 defined-unary-op
definedUnaryOp :: Parser (Expression -> Expression)
definedUnaryOp = do
  char '.'
  opname <- many1 letter
  char '.'
  spaces
  return $ UnaryOperand $ DefinedUnaryOp opname

-- | R705 mult-operand
multOperand :: Parser Expression
multOperand = level1Expr `chainl1` powOp

-- | R706 add-operand
addOperand :: Parser Expression
addOperand = multOperand `chainl1` multOp

-- | R707 level-2-expr
level2Expr :: Parser Expression
level2Expr = do
  unaryOp <- optionMaybe (unaryAddOp)
  rest <- addOperand `chainl1` addOp
  case unaryOp of
    Nothing -> return rest
    Just (uop) -> return $ replaceFirstPrimary uop rest
  where replaceFirstPrimary unaryOp expr = case expr of
          BinaryOperand binop expr1 expr2 -> BinaryOperand binop (replaceFirstPrimary unaryOp expr1) expr2
          _ -> unaryOp expr
  -- TODO fix, this code does not work well (ex) "-(a+a)" seems  "(-a)+a"


unaryAddOp :: Parser (Expression -> Expression)
unaryAddOp = (try $ do { string "+";  spaces; return $ UnaryOperand UnaryAdd})
         <|> (try $ do { string "-";  spaces; return $ UnaryOperand UnarySub})

-- | R708
powOp :: Parser (Expression -> Expression -> Expression)
powOp = try $ do { spaces; string "**"; spaces; return $ BinaryOperand Pow}

-- | R709
multOp :: Parser (Expression -> Expression -> Expression)
multOp = (try $ do { spaces; string "*";  spaces; return $ BinaryOperand Mul})
     <|> (try $ do { spaces; string "/";  spaces; return $ BinaryOperand Div})

-- | R710
addOp :: Parser (Expression -> Expression -> Expression)
addOp = (try $ do { spaces; string "+";  spaces; return $ BinaryOperand Add})
    <|> (try $ do { spaces; string "-";  spaces; return $ BinaryOperand Sub})


-- | R711 level-3-expr
level3Expr :: Parser Expression
level3Expr = level2Expr `chainl1` concatOp

-- | R712
concatOp :: Parser (Expression -> Expression -> Expression)
concatOp = try $ do { spaces; string "//"; spaces; return $ BinaryOperand Concat}

-- | R713 level-4-expr
level4Expr :: Parser Expression
level4Expr = do
  first <- level3Expr
  rest <- optionMaybe( do { rel <- relOp; arg2 <- level3Expr;return $ (\x -> rel x arg2) })
  case rest of
    Just ( op ) -> return $ op first
    Nothing -> return $ first

-- | R714 rel-op
relOp :: Parser (Expression -> Expression -> Expression)
relOp = do
  spaces
  op <- choice [
    do { string ".eq."; return $ BinaryOperand Equal },
    do { string "=="; return $ BinaryOperand Equal },
    do { string ".neq."; return $ BinaryOperand NEqual },
    do { string "/="; return $ BinaryOperand NEqual },
    do { string ".lt."; return $ BinaryOperand LessT },
    do { string "<"; return $ BinaryOperand LessT },
    do { string ".le."; return $ BinaryOperand LessE },
    do { string "<="; return $ BinaryOperand LessE },
    do { string ".gt."; return $ BinaryOperand GreT },
    do { string ">"; return $ BinaryOperand GreT },
    do { string ".ge."; return $ BinaryOperand GreE },
    do { string ">="; return $ BinaryOperand GreE }
               ]
  spaces
  return op

-- | R715 and-operand
andOperand :: Parser Expression
andOperand = try (do { op <- notOp; spaces; arg <- level4Expr; return $ UnaryOperand Not arg}) <|> level4Expr

-- | R716 or-operand
orOperand :: Parser Expression
orOperand = andOperand `chainl1` andOp

-- | R717 equiv-operand
equivOperand :: Parser Expression
equivOperand = orOperand `chainl1` orOp

-- | R718 level5-expr
level5Expr :: Parser Expression
level5Expr = equivOperand `chainl1` equivOp

-- | R719
notOp :: Parser (Expression -> Expression)
notOp = try $ do { spaces; string ".not."; spaces; return $ UnaryOperand Not }

-- | R720
andOp :: Parser (Expression -> Expression -> Expression)
andOp = try $ do { spaces; string ".and."; spaces; return $ BinaryOperand And}

-- | R721
orOp :: Parser (Expression -> Expression -> Expression)
orOp = try $ do { spaces; string ".or."; spaces; return $ BinaryOperand Or}

-- | R722
equivOp :: Parser (Expression -> Expression -> Expression)
equivOp = (try $ do { spaces; string ".neqv.";  spaces; return $ BinaryOperand NEquiv})
      <|> (try $ do { spaces; string ".eqv.";  spaces; return $ BinaryOperand Equiv})

-- | R723 expr
expression :: Parser Expression
expression = level5Expr `chainl1` definedBinaryOp

-- | R704 defined-binary-op
definedBinaryOp :: Parser (Expression -> Expression -> Expression)
definedBinaryOp = do
  spaces
  char '.'
  opname <- many1 letter
  char '.'
  spaces
  return $ BinaryOperand $ DefinedBinaryOp opname


-- | R735 assignment-stmt
assignmentStatement :: Parser FortranExecute
assignmentStatement = line $ do
  lhs <- variable
  spaces
  char '='
  spaces
  expr <- expression
  return $ Assignment lhs expr

-- | R911 print-stmt (WIP)
printStatement :: Parser FortranExecute
printStatement = line $ do
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
               <|> try assignmentStatement
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
