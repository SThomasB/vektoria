module Lib.VParse
  ( vektoriaParse
  , run
  ) where

import Data.Char
import Lib.Data.Token
import Lib.Parser.Parser

-- bin ::= term + expr | expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | int
vektoriaParse :: Parser [Token] Statement
vektoriaParse = do
  statement

statement :: Parser [Token] Statement
statement = do assignStatement <|> printStatement <|> weakStatement

assignStatement :: Parser [Token] Statement
assignStatement = do
  identifier <- matchSymbol SIdentifier
  matchSymbol SEqual
  expr <- expression
  let entityName = lexeme identifier
  return (Assign $ Entity entityName expr)

printStatement :: Parser [Token] Statement
printStatement = do
  matchSymbol SPrint
  expr <- expression
  return (Print $ expr)

weakStatement :: Parser [Token] Statement
weakStatement = do
  expr <- expression
  return $ Weak expr

expression :: Parser [Token] Expression
expression =
  binaryExpression
    [ SPlus
    , SMinus
    , SRight
    , SLeft
    , SLeftEqual
    , SRightEqual
    , SEqualEqual
    , SBarBar
    , SAndAnd
    , SSlashEqual
    ]
    term

binaryExpression ::
     [Symbol] -> Parser [Token] Expression -> Parser [Token] Expression
binaryExpression syms operand = do
  left <- operand
  rest <-
    many $ do
      token <- matchOneOf syms
      let op = getOperator token
      right <- operand
      return (op, right)
  return $ foldl (\acc (op, expr) -> Binary op acc expr) left rest

operator :: Parser [Token] Operator
operator = do
  token <- next
  let op = getOperator token
  return op

isLiteral :: Token -> Bool
isLiteral token = (element token) /= EVoid

matchSymbol :: Symbol -> Parser [Token] Token
matchSymbol sym = do
  token <- next
  if (match sym token)
    then return token
    else empty

matchOneOf :: [Symbol] -> Parser [Token] Token
matchOneOf syms = do
  token <- next
  if ((symbol token) `elem` syms)
    then return token
    else empty

term :: Parser [Token] Expression
term = binaryExpression [SStar, SSlash] factor

factor :: Parser [Token] Expression
factor = literalExpr <|> parenExpr

parenExpr :: Parser [Token] Expression
parenExpr = do
  matchSymbol SLeftParen
  expr <- expression
  matchSymbol SRightParen
  return expr

literalExpr :: Parser [Token] Expression
literalExpr = do
  token <- next
  if (match SIdentifier token)
    then return (Ref (lexeme token))
    else if (isLiteral token)
           then return (ElemExpr (element token))
           else empty

parseOp :: Operator -> Parser [Token] Operator
parseOp thisOp = do
  n <- next
  let op = getOperator n
  if (thisOp == op)
    then return op
    else empty

next :: Parser [Token] Token
next =
  Parser $ \tokens ->
    case tokens of
      [] -> []
      (result:remaining) -> [(result, remaining)]
