module Elaine.Parse (parseProgram, parseExpr, ParseResult, Spans, Span) where

import Control.Monad.State (State, evalState, get, put, runState)
import Data.Aeson (ToJSON)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void
import Elaine.AST
import Elaine.Ident (Ident (Ident, idText), Location (LocOffset, LocNone))
import GHC.Generics (Generic)
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, try),
    ParseErrorBundle,
    ParsecT,
    between,
    empty,
    errorBundlePretty,
    getInput,
    getOffset,
    many,
    manyTill,
    oneOf,
    option,
    optional,
    runParserT,
    sepEndBy,
    (<|>),
  )
import Text.Megaparsec.Char (char, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (span)
import Elaine.TypeVar (TypeVar(ExplicitVar))

data Span = Span Int Int String
  deriving (Show, Generic)

instance ToJSON Span

type Spans = [(Span, String)]

type Parser = ParsecT Void Text (State Spans)

type ParseResult a = Either (ParseErrorBundle Text Void) a

trim :: String -> String
trim = reverse . f . reverse . f
  where
    f = dropWhile isSpace

getSpan :: Parser a -> Parser (a, Span)
getSpan p = do
  start <- getOffset
  input <- fmap unpack getInput
  x <- p
  end <- getOffset
  let length' = end - start
  let span' = Span start end (take length' input)

  return (x, span')

saveSpan :: String -> Span -> Parser ()
saveSpan c s = do
  currentList <- get
  put (currentList ++ [(s, c)])

span :: String -> Parser a -> Parser a
span c p = do
  (res, s) <- getSpan p
  () <- saveSpan c s
  return res

parseProgram :: (Text, Text) -> Either String ([Declaration], Spans)
parseProgram (name, s) =
  let parsed = runParserT program (unpack name) s
      afterState = runState parsed []
   in case afterState of
        (Left a, _) -> Left (errorBundlePretty a)
        (Right p, s') -> Right (p, s')

-- This mostly exists for testing
parseExpr :: String -> ParseResult Expr
parseExpr s =
  let parsed = runParserT expr s (pack s)
   in evalState parsed empty

-- WHITESPACE & BASIC SYMBOLS
space' :: Parser ()
space' =
  L.space
    space1
    (span "comment.line.number-sign.elaine" $ L.skipLineComment "#")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space'

symbol :: Text -> Parser Text
symbol = L.symbol space'

keyword :: Text -> Parser Text
keyword kw = span "keyword" $ lexeme (string kw <* notFollowedBy (oneOf otherIdentChars))

delimiters :: Text -> Text -> Parser a -> Parser a
delimiters left right = between (symbol left) (symbol right)

parens :: Parser a -> Parser a
parens = delimiters "(" ")"

braces :: Parser a -> Parser a
braces = delimiters "{" "}"

brackets :: Parser a -> Parser a
brackets = delimiters "[" "]"

angles :: Parser a -> Parser a
angles = delimiters "<" ">"

semicolon :: Parser Text
semicolon = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

equals :: Parser Text
equals = symbol "="

-- IDENTIFIERS
-- Identifiers can contain numbers, but may not start with them
-- They may contain underscores, lowercase letters and uppercase letters
-- They may end with ticks, like in Haskell
-- They match the regex [a-zA-Z_][a-zA-Z_0-9]*'*
firstIdentChars :: [Char]
firstIdentChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']

otherIdentChars :: [Char]
otherIdentChars = firstIdentChars ++ ['0' .. '9']

identCategory :: String -> String
identCategory "resume" = "variable.language.elaine"
identCategory _ = "variable.elaine"

categorizeIdent :: Parser Ident -> Parser Ident
categorizeIdent p = do
  (ident', s) <- getSpan p

  () <- saveSpan (identCategory $ trim $ idText ident') s

  return ident'

ident :: Parser Ident
ident = categorizeIdent $ lexeme $ do
  location <- LocOffset <$> getOffset
  firstChar <- oneOf firstIdentChars
  rest <- many (oneOf otherIdentChars)
  ticks <- many (char '\'')
  exc <- option "" (symbol "!")
  return $ Ident ([firstChar] ++ rest ++ ticks ++ unpack exc) location

typeIdent :: Parser Ident
typeIdent = span "storage.type.elaine" $ lexeme $ do
  location <- LocOffset <$> getOffset
  firstChar <- oneOf firstIdentChars
  rest <- many (oneOf otherIdentChars)
  ticks <- many (char '\'')
  exc <- option "" (symbol "!")
  return $ Ident ([firstChar] ++ rest ++ ticks ++ unpack exc) location

-- PROGRAM
-- A program is simply a sequence of modules, but the parser requires parsing
-- the entire source code.
program :: Parser [Declaration]
program = space' *> many declaration <* eof

mod' :: Parser DeclarationType
mod' = do
  name <- keyword "mod" >> ident
  decs <- braces (many declaration)
  return $ Module name decs

-- DECLARATIONS
declaration :: Parser Declaration
declaration = do
  vis <- visibility
  Declaration vis
    <$> ( decUse
            <|> mod'
            <|> decVal
            <|> decType
            <|> decEffect
        )

visibility :: Parser Visibility
visibility = Public <$ try (keyword "pub") <|> return Private

decUse :: Parser DeclarationType
decUse = Use <$> try (keyword "use" >> ident <* semicolon)

elaboration :: Parser Expr
elaboration = do
  from <- try (keyword "elaboration") >> ident
  to <- symbol "->" >> row
  Val . Elb . Elaboration from to <$> braces (many operationClause)

decVal :: Parser DeclarationType
decVal = do
  (name, rec', t, e) <- letRecLike
  _ <- semicolon
  return $ DecLet name rec' t e

function :: Parser Function
function = Function <$> functionParams <*> optional computationType <*> exprBlock

handler :: Parser Expr
handler = do
  arms <- try (keyword "handler") >> braces (many handlerArm)
  let (rets, functions) = partitionEithers arms
      ret = case rets of
        [r] -> Just r
        [] -> Nothing
        _ -> error "Handler cannot have multiple return arms"
  return $ Val $ Hdl $ Handler ret functions

handlerArm :: Parser (Either Function OperationClause)
handlerArm =
  (Left <$> handleReturn)
    <|> (Right <$> operationClause)

handleReturn :: Parser Function
handleReturn = try (keyword "return") >> function

operationClause :: Parser OperationClause
operationClause = OperationClause <$> ident <*> parens (ident `sepEndBy` comma) <*> exprBlock

decEffect :: Parser DeclarationType
decEffect = do
  name <- try (keyword "effect") >> ident
  DecEffect name <$> braces (many operationSignature)

operationSignature :: Parser OperationSignature
operationSignature = OperationSignature <$> ident <*> parens (computationType `sepEndBy` comma) <*> computationType

decType :: Parser DeclarationType
decType = do
  name <- try (keyword "type") >> ident
  params <- fromMaybe [] <$> optional (brackets (ident `sepEndBy` comma))
  let params' = map ExplicitVar params
  DecType name params' <$> braces (constructor `sepEndBy` comma)

functionParams :: Parser [(Ident, Maybe ASTComputationType)]
functionParams = parens (functionParam `sepEndBy` comma)

constructor :: Parser Constructor
constructor = Constructor <$> ident <*> parens (computationType `sepEndBy` comma)

functionParam :: Parser (Ident, Maybe ASTComputationType)
functionParam = do
  name <- ident
  typ' <- optional (colon >> computationType)
  return (name, typ')

computationType :: Parser ASTComputationType
computationType = do
  effs <- optional row
  ASTComputationType (fromMaybe (Row [] Nothing) effs) <$> valueType

valueType :: Parser ASTValueType
valueType =
  try (parens $ pure TypeUnit)
    <|> functionType
    <|> typeConstructor
    -- It's important to try parens valueType first, because
    -- it is also a valid tuple type expression
    <|> try (parens valueType)
    <|> tupleType

tupleType :: Parser ASTValueType
tupleType = TypeTuple <$> parens (valueType `sepEndBy` comma)

typeConstructor :: Parser ASTValueType
typeConstructor = do
  id' <- typeIdent
  params <- fromMaybe [] <$> optional (brackets $ valueType `sepEndBy` comma)
  return $ TypeConstructor id' params

functionType :: Parser ASTValueType
functionType = try (keyword "fn") >> TypeArrow <$> parens (computationType `sepEndBy` comma) <*> computationType

row :: Parser Row
row =
  angles $ do
    effects <- ident `sepEndBy` comma
    extend <- optional (symbol "|" >> ident)
    return $ Row effects extend

-- EXPRESSIONS
data LetOrExpr = Let' Ident Rec (Maybe ASTComputationType) Expr | Expr' Expr

exprBlock :: Parser Expr
exprBlock = do
  exprs <- braces (letOrExpr `sepEndBy` semicolon)
  case foldr f Nothing exprs of
    Just a -> return a
    Nothing -> return $ Val Unit
  where
    f (Expr' e1) Nothing = Just e1
    f (Let' {}) Nothing = error "last expression in a block cannot be let"
    f (Expr' e1) (Just e2) = Just $ Let Nothing Nothing e1 e2
    f (Let' x r t e1) (Just e2) = Just $ Let (Just (x, r)) t e1 e2

letOrExpr :: Parser LetOrExpr
letOrExpr = let' <|> Expr' <$> expr

expr :: Parser Expr
expr = do
  root <-
    exprBlock
      <|> if'
      <|> try (Val <$> value)
      <|> match'
      <|> handle
      <|> elab
      <|> handler
      <|> elaboration
      <|> listLit
      <|> (Var <$> ident)
      <|> (Tuple <$> parens (expr `sepEndBy` comma))
  applications <- many (parens (expr `sepEndBy` comma))
  return $ foldl App root applications

listLit :: Parser Expr
listLit = do
  exprs <- brackets (expr `sepEndBy` comma)
  return $ foldr
    (\a b -> App (Var $ Ident "Cons" LocNone) [a, b])
    (App (Var $ Ident "Nil" LocNone) []) exprs

value :: Parser Value
value =
  (Int <$> intLiteral)
    <|> (String <$> stringLiteral)
    <|> (Bool <$> boolLiteral)
    <|> (Unit <$ unitLiteral)
    <|> (Fn <$> functionLiteral)

let' :: Parser LetOrExpr
let' = do
  (x, r, t, e) <- letRecLike
  return $ Let' x r t e

letRecLike :: Parser (Ident, Rec, Maybe ASTComputationType, Expr)
letRecLike = do 
  recMaybe <- try (keyword "let") >> optional (keyword "rec")
  let rec' = case recMaybe of 
        Nothing -> NotRec
        Just _ -> Rec
  name <- ident
  t <- optional (symbol ":" >> computationType)
  e <- equals >> expr
  return (name, rec', t, e)

if' :: Parser Expr
if' = do
  cond <- try (keyword "if") >> expr
  e1 <- exprBlock
  e2 <- keyword "else" >> exprBlock
  return $ If cond e1 e2

handle :: Parser Expr
handle = try (keyword "handle") >> Handle <$> brackets expr <*> expr

elab :: Parser Expr
elab = do
  _ <- try (keyword "elab")
  elaborationExpr <- optional (brackets expr)
  case elaborationExpr of
    Just a -> Elab a <$> expr
    Nothing -> do
      -- Implicit elabs need a unique identifier for the transformation
      -- The offset is unique, so might as well use that.
      off <- getOffset
      ImplicitElab off <$> expr

match' :: Parser Expr
match' = do
  l <- try (keyword "match") >> expr
  Match l <$> braces (matchArm `sepEndBy` comma)

matchArm :: Parser MatchArm
matchArm = do
  name <- ident
  p <- parens (ident `sepEndBy` comma)
  e <- symbol "=>" >> expr
  return $ MatchArm (Pattern name p) e

-- Literals
intLiteral :: Parser Int
intLiteral = span "constant.numeric.elaine" $ do
  negative <- optional (symbol "-")
  let mul = case negative of
        Just _ -> negate
        Nothing -> id
  mul <$> lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = span "string.quoted.elaine" $ char '\"' *> manyTill L.charLiteral (char '\"') <* space

boolLiteral :: Parser Bool
boolLiteral = span "constant.language.boolean.elaine" $ True <$ try (keyword "true") <|> False <$ try (keyword "false")

unitLiteral :: Parser ()
unitLiteral = span "constant.language.unit.elaine" $ parens $ pure ()

functionLiteral :: Parser Function
functionLiteral = try (keyword "fn") >> function
