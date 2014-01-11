module HFortran.Parser where
import HFortran.AST

import Text.Parsec hiding (space, spaces)
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

fortranCode :: Parser [FortranTopLevel]
fortranCode = undefined

fortranTopLevel :: Parser FortranTopLevel
fortranTopLevel = undefined

space :: Parser ()
space = oneOf " \t" >> return ()

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space

eol :: Parser ()
eol = do {_ <- char '\n'; return ()} <|> eof

braced :: Parser a -> Parser a
braced = between (do { _ <- char '(';  spaces}) (do {spaces; _ <- char ')'; return ()})

identifier :: Parser Symbol
identifier = (:) <$> letter <*> (many $ letter <|> digit) >>= return . Symbol

-- | R207 declarationConstruct
declarationStatement :: Parser FortranDeclaration
declarationStatement = do try (typeDeclarationStatement)

-- | R304 name
-- TODO length check
name :: Parser Symbol
name = (:) <$> letter <*> (many $ letter <|> digit <|> (char '_')) >>= return . Symbol

-- | parse a fortran line
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
           _ <- char ','
           spaces

-- | R401 signed-digit-string
signedDigitString :: Parser Symbol
signedDigitString = do
  signOption <- optionMaybe sign
  digits <- digitString
  case signOption of
    Nothing -> return digits
    Just x -> return $ Symbol ( x : symbolContent digits )

-- | R402 digit-string
digitString :: Parser Symbol
digitString = many1 digit >>= return . Symbol

-- | R403 signed-int-literal-constant
signedIntLiteralConstant :: Parser FortranConstant
signedIntLiteralConstant = do
  signOption <- optionMaybe sign
  digitBody <- digitString
  kind <- optionMaybe $ char '_' >> kindParam 
  let value = readSymbol digitBody :: Int
    in
    case signOption of
      Nothing -> return $ IntLiteralConstant value kind
      Just x -> return $ IntLiteralConstant (value * (if x == '+' then 1 else -1)) kind

-- | R404 int-literal-constant
intLiteralConstant :: Parser FortranConstant
intLiteralConstant = do
  digit <- digitString
  kind <- optionMaybe $ char '_' >> kindParam 
  return $ IntLiteralConstant (readSymbol digit) kind

-- | R405 kind-param
-- TODO accept named constant
kindParam :: Parser FortranConstant
kindParam = do
  digit <- digitString
  return $ IntLiteralConstant (readSymbol digit) Nothing

-- | R406 sign
sign :: Parser Char
sign = char '+' <|> char '-'

-- | R420 char-literal-constant
charLiteralConstant :: Parser FortranConstant
charLiteralConstant = (do { _ <- char '"'; str <- many (noneOf "\""); _ <- char '"'; return $ CharLiteralConstant str })
                  <|> (do { _ <- char '\''; str <- many (noneOf "\'"); _ <- char '\''; return $ CharLiteralConstant str })

constant :: Parser FortranConstant
constant = signedIntLiteralConstant <|> charLiteralConstant

-- | R501 type-declaration-stmt
typeDeclarationStatement :: Parser FortranDeclaration
typeDeclarationStatement = line $ do
  baseType <- typeSpec
  spaces
  attrs <- optionMaybe $ do{ 
    spaces;
    attrs <- many $ do {
      _ <- char ',';
      spaces;
      attr <- attrSpec;
      return attr};
    spaces ;
    _ <- string "::";
    return attrs}
  spaces
  ids <- sepBy identifier commaSep
  return $ TypeDeclaration baseType attrs ids

-- | R502 type-spec
typeSpec :: Parser FortranBaseType
typeSpec = (string "integer" >> return FInteger)
       <|> (string "real" >> return FReal)
       <|> (string "double" >> spaces >> string "precision" >> return FDoublePrecision)
       <|> (string "complex" >> return FComplex )
       <|> (string "character" >> return FCharacter)
       <|> (string "logical" >> return FLogical )
       <|> (string "type" >> spaces >> braced identifier >> return FType)

-- | R503 + R511
attrSpec :: Parser Attr
attrSpec = (try $ string "parameter" >> return (Type Parameter))
       <|> (try $ string "public" >> return (Access Public))
       <|> (try $ string "private" >> return (Access Private))
       <|> (string "allocatable" >> return (Type Allocatable))
       <|> (string "dimension" >> spaces >> braced arraySpec >>= return . Type . Dimension) -- TODO fix
       <|> (string "external" >> return (Type External))
       <|> (try $ string "intent" >> spaces >> braced intentSpec >>= return . Type . Intent)
       <|> (string "intrinsic" >> return (Type Intrinsic))
       <|> (string "optional" >> return (Type Optional))
       <|> (string "pointer" >> return (Type Pointer))
       <|> (string "save" >> return (Type Save))
       <|> (string "target" >> return (Type Target))

-- | R502 intent-spec
intentSpec :: Parser IntentSpec
intentSpec = (try $ string "in" >> return In)
       <|> (string "out" >> return Out)
       <|> (string "inout" >> return InOut)


-- TODO, check AssumedSizeSpec can be appear at the last of list
-- | R513 array-spec
arraySpec :: Parser [ArraySpec]
arraySpec = try (explicitShapeSpec `sepBy` commaSep)
            <|> try (assumedShapeSpec `sepBy` commaSep)

-- | R514 explicit-shape-spec
-- R515 lowerBound
-- R516 upperBound
-- R519 assumed-size-spec
explicitShapeSpec :: Parser ArraySpec
explicitShapeSpec = do
  lower <- optionMaybe $ try $ do
    expr <- expression
    spaces
    _ <- char ':'
    return expr
  upper <- (do { expr <- expression; return $ \lower -> ExplicitArraySpec lower expr} )
       <|> (do { _ <- char '*'; return AssumedSize } )
  return $ upper lower

-- | R517 assumed-shape-spec
assumedShapeSpec :: Parser ArraySpec
assumedShapeSpec = do
  lower <- optionMaybe $ try (do
    expr <- expression
    spaces
    return expr)
  _ <- char ':'
  return $ AssumedArraySpec lower

-- | R518 deferred-shape-spec
deferredShapeSpec :: Parser ArraySpec
deferredShapeSpec = char ':' >> return DeferredArraySpec

-- not work well
-- -- | R519 assumed-size-spec
-- assumedSizeSpec :: Parser [ArraySpec]
-- assumedSizeSpec = try $ do
--   first <- try $ explicitShapeSpec `sepBy` commaSep
--   spaces
--   char ','
--   spaces
--   -- lowerBound <- optionMaybe ( do { expr <- expression; spaces; char ':'; return expr } )
--   char '*'
--   return $ first ++ []
--   -- return $ first ++ [AssumedSize lowerBound]

-- -- | R541
-- implictStatement :: Parser 

-- R601 variable
variable :: Parser Expression
variable = name >>= return . Variable -- scalar-variable-name or array-variable-name

-- | R701 Primary
primary :: Parser Expression
primary = 
      (try functionReference)
--       <|> constantSubobject
      <|> (try variable)
--       <|> arrayConstructor
--       <|> structConstructor
      <|> (braced expression)
      <|> (try $ constant >>= return . Constant)

-- R702 constant subobject

-- | R703
level1Expr :: Parser Expression
level1Expr = do
  first <- optionMaybe(definedUnaryOp)
  spaces
  rest <- primary
  case first of
    Nothing -> return rest
    Just (duop) -> return $ duop rest

-- | R704 defined-unary-op
definedUnaryOp :: Parser (Expression -> Expression)
definedUnaryOp = try $ do
  _ <- char '.'
  opname <- many1 letter
  _ <- char '.'
  spaces
  return $ UnaryOperand $ DefinedUnaryOp $ Symbol $ "." ++ opname ++ "."

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
unaryAddOp = (try $ char '+' >> spaces >> return (UnaryOperand UnaryAdd))
         <|> (try $ char '-' >> spaces >> return (UnaryOperand UnarySub))

-- | R708
powOp :: Parser (Expression -> Expression -> Expression)
powOp = try $ spaces >> string "**" >> spaces >> return (BinaryOperand Pow)

-- | R709
multOp :: Parser (Expression -> Expression -> Expression)
multOp = (try $ try spaces >> char '*' >>  spaces >> return (BinaryOperand Mul))
     <|> (try $ try spaces >> char '/' >>  spaces >> return (BinaryOperand Div))

-- | R710
addOp :: Parser (Expression -> Expression -> Expression)
addOp = (try $ try spaces >> char '+' >> spaces >> return (BinaryOperand Add))
    <|> (try $ try spaces >> char '-' >> spaces >> return (BinaryOperand Sub))


-- | R711 level-3-expr
level3Expr :: Parser Expression
level3Expr = level2Expr `chainl1` concatOp

-- | R712
concatOp :: Parser (Expression -> Expression -> Expression)
concatOp = try $ try spaces >> string "//" >> spaces >> return (BinaryOperand Concat)

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
relOp = try $ do
  spaces
  op <- choice [
    string ".eq." >> return ( BinaryOperand Equal ),
    string "==" >> return ( BinaryOperand Equal ),
    string ".neq." >> return ( BinaryOperand NEqual ),
    string "/=" >> return ( BinaryOperand NEqual ),
    string ".lt." >> return ( BinaryOperand LessT ),
    string "<" >> return ( BinaryOperand LessT ),
    string ".le." >> return ( BinaryOperand LessE ),
    string "<=" >> return ( BinaryOperand LessE ),
    string ".gt." >> return ( BinaryOperand GreT ),
    string ">" >> return ( BinaryOperand GreT ),
    string ".ge." >> return ( BinaryOperand GreE ),
    string ">=" >> return ( BinaryOperand GreE )
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
notOp = try $ do { try spaces; _ <- string ".not."; spaces; return $ UnaryOperand Not }

-- | R720
andOp :: Parser (Expression -> Expression -> Expression)
andOp = try $ do { try spaces; _ <- string ".and."; spaces; return $ BinaryOperand And}

-- | R721
orOp :: Parser (Expression -> Expression -> Expression)
orOp = try $ do { try spaces; _ <- string ".or."; spaces; return $ BinaryOperand Or}

-- | R722
equivOp :: Parser (Expression -> Expression -> Expression)
equivOp = (try $ do { try spaces; _ <- string ".neqv.";  spaces; return $ BinaryOperand NEquiv})
      <|> (try $ do { try spaces; _ <- string ".eqv.";  spaces; return $ BinaryOperand Equiv})

-- | R723 expr
expression :: Parser Expression
expression = try $ do {ret <- level5Expr `chainl1` definedBinaryOp; spaces; return ret}

-- | R704 defined-binary-op
definedBinaryOp :: Parser (Expression -> Expression -> Expression)
definedBinaryOp = try $ do
  spaces
  _ <- char '.'
  opname <- many1 letter
  _ <- char '.'
  spaces
  return $ BinaryOperand $ DefinedBinaryOp $ Symbol opname


-- | R735 assignment-stmt
assignmentStatement :: Parser FortranExecute
assignmentStatement = line $ do
  lhs <- variable
  spaces
  _ <- char '='
  spaces
  expr <- expression
  return $ Assignment lhs expr

-- | R911 print-stmt (WIP)
printStatement :: Parser FortranExecute
printStatement = line $ do
  _ <- string "print"
  spaces
  fmt <- format
  spaces
  outputItemList <- many $ try $ do { commaSep ; item <- charLiteralConstant; return item }
  return $ Print fmt outputItemList

-- | R913 format
format :: Parser FortranFormat
format = (do _ <- char '*'; return FormatAsterisc)
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
  _ <- string "continue"
  return $ Continue

executeStatement :: Parser FortranExecute
executeStatement = try continueStatement
               <|> try assignmentStatement
               <|> try printStatement

programStatement :: Parser Symbol
programStatement = line $ do
  _ <- string "program"
  spaces
  programName <- identifier
  return $ programName

endProgramStatement :: String -> Parser ()
endProgramStatement programName = line $ do
  _ <- string "end"
  spaces
  _ <- string "program"
  spaces
  notFollowedBy alphaNum <|> do {_ <- string programName; return ()} <?> "require end program " ++ programName
  return ()

-- | R1101 main-program
program :: Parser FortranTopLevel
program = do
  programName <- programStatement
  decls <- many declarationStatement
  exes <- many executeStatement
  endProgramStatement $ symbolContent programName
  return $ Program programName decls exes

-- | R1210 function-reference
functionReference :: Parser Expression
functionReference = do
  functionName <- identifier
  spaces
  arguments <- braced $ sepBy expression commaSep
  return $ FunctionReference functionName arguments

-- TODO R1212 actual-arg-spec
-- R1213
-- R1214 