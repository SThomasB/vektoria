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
vektoriaParse :: Parser [Token] Expression
vektoriaParse = do
  expression

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
  if (isLiteral token)
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
