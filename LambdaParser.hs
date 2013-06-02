module LambdaParser where

import Prelude
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (haskellDef)

import TypeDefs

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where 
    style = haskellDef {Tok.reservedOpNames = [".","\\"] }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

allOf :: Parser a -> Parser a
allOf p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

variable :: Parser LambdaTerm
variable = do
  x <- identifier
  return (Var x)

varlist :: Parser [VarName]
varlist = many1 identifier

lambda :: Parser LambdaTerm
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "."
  e <- expr
  return (Abs x e)

mlambda :: Parser LambdaTerm
mlambda = do
  reservedOp "\\"
  xs <- varlist
  reservedOp "."
  e <- expr
  return (MultiAbs xs e)

factor :: Parser LambdaTerm
factor =  parens expr
      <|> variable
      <|> mlambda
      <|> lambda
      <?> "lambda term"

term :: Parser LambdaTerm
term = Ex.buildExpressionParser [] factor

expr :: Parser LambdaTerm
expr = do
  es <- many1 term
  return (foldl1 App es)

parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm t = 
  case parse (allOf expr) "" t of
    Left err -> error (show err)
    Right ast -> ast

