module Vektoria.Parser.VParse
  ( vektoriaParse
  , run
  ) where

import Data.Char
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.ParsingGenerics

getOperator :: Token -> Operator
getOperator token =
  case (symbol token) of
    SPlus -> Plus
    SMinus -> Minus
    SEqualEqual -> Equals
    SSlashEqual -> NotEquals
    SAndAnd -> And
    SBarBar -> Or
    SLeft -> Less
    SRight -> Greater
    SLeftEqual -> SubSet
    SRightEqual -> SuperSet
    SStar -> Multiply
    SSlash -> Divide
    _ -> NoOp

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
  identifier <- symbolSatisfy (==SIdentifier)
  symbolSatisfy (==SEqual)
  expr <- expression
  let entityName = lexeme identifier
  return (Assign $ Entity entityName expr)

printStatement :: Parser [Token] Statement
printStatement = do
  symbolSatisfy (==SPrint)
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
      token <- symbolSatisfy (oneOf syms)
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


term :: Parser [Token] Expression
term = binaryExpression [SStar, SSlash] factor

factor :: Parser [Token] Expression
factor = literalExpr <|> parenExpr

parenExpr :: Parser [Token] Expression
parenExpr = do
  symbolSatisfy (==SLeftParen)
  expr <- expression
  symbolSatisfy (==SRightParen)
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


symbolSatisfy :: (Symbol->Bool) -> Parser [Token] Token
symbolSatisfy = satisfy symbol

oneOf :: Eq a => [a] -> (a -> Bool)
oneOf = flip elem

