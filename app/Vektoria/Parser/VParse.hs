module Vektoria.Parser.VParse
  ( vektoriaParse
  , run
  ) where

import Data.Char
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Expression
import Vektoria.Lib.Data.Element
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
  statement <|> block

block :: Parser [Token] Statement
block = do
  symbolSatisfy (== SLeftBrace)
  result <- (many vektoriaParse)
  symbolSatisfy (== SRightBrace)
  return $ Block result

statement :: Parser [Token] Statement
statement = do
  assignStatement <|> ifElseStatement <|> printStatement

ifElseStatement :: Parser [Token] Statement
ifElseStatement =
  do symbolSatisfy (== SIf)
     condition <- expression
     thenBlock <- do block <|> printStatement
     symbolSatisfy (== SElse)
     elseBlock <- do block <|> ifElseStatement <|> printStatement
     return $ IfElse condition thenBlock elseBlock
     <|> do
    symbolSatisfy (== SIf)
    condition <- expression
    thenBlock <- do block <|> printStatement
    return $ IfElse condition thenBlock (Block [])

assignStatement :: Parser [Token] Statement
assignStatement = do
  identifier <- symbolSatisfy (== SIdentifier)
  symbolSatisfy (== SEqual)
  expr <- expression <|> lambda
  let entityName = lexeme identifier
  return (Assign entityName expr)

printStatement :: Parser [Token] Statement
printStatement = do
  symbolSatisfy (== SPrint)
  expr <- expression
  return (Print $ expr)

weakStatement :: Parser [Token] Statement
weakStatement = do
  symbolSatisfy (== SLeftArrow)
  expr <- expression
  return $ Weak expr

timesBracket :: Parser [Token] Expression
timesBracket = do
  left <- factor
  symbolSatisfy (== SLeftBracket)
  right <- expression
  symbolSatisfy (== SRightBracket)
  return $ Binary Multiply left right

functionCall :: Parser [Token] Expression
functionCall = do
  symbolSatisfy (==SLeftParen)
  callee <- lambda <|> reference
  args <- some expression
  symbolSatisfy (==SRightParen)
  case callee of
    (Lambda parameters expression) -> return $ Call (Lambda parameters expression) args
    (Ref ref) -> return $ Call (Ref ref) args


lambda :: Parser [Token] Expression
lambda = do
  symbolSatisfy (==SLeftParen)
  parameters <- many $ symbolSatisfy (==SIdentifier)
  let parameters' = map lexeme parameters
  symbolSatisfy (==SComma)
  expression <- expression
  symbolSatisfy (==SRightParen)
  return $ Lambda parameters' expression


expression :: Parser [Token] Expression
expression = do
  timesBracket
  <|> binaryExpression
    [ Plus
    , Minus
    , Greater
    , Less
    , SubSet
    , SuperSet
    , Equals
    , Or
    , And
    , NotEquals
    ]
    term


binaryExpression ::
     [Operator] -> Parser [Token] Expression -> Parser [Token] Expression
binaryExpression operators operand = do
  left <- operand
  rest <-
    many $ do
      op <- operatorSatisfy (isOneOf operators)
      right <- operand
      return (op, right)
  return $ foldl (\acc (op, expr) -> Binary op acc expr) left rest

term :: Parser [Token] Expression
term = binaryExpression [Multiply, Divide] factor

factor :: Parser [Token] Expression
factor = functionCall<|>literalExpr <|> parenExpr

parenExpr :: Parser [Token] Expression
parenExpr = do
  symbolSatisfy (== SLeftParen)
  expr <- expression
  symbolSatisfy (== SRightParen)
  return expr

reference :: Parser [Token] Expression
reference = do
  token <- symbolSatisfy (== SIdentifier)
  return $ Ref (lexeme token)

literalExpr :: Parser [Token] Expression
literalExpr = do
  reference
  <|> do
    token <- symbolSatisfy (== SString)
    return $ ElemExpr (EString ((init . tail) $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SInt)
    return $ ElemExpr (EInt (read $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SFloat)
    return $ ElemExpr (EFloat (read $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SFalse)
    return $ ElemExpr (EBool False)
  <|> do
    token <- symbolSatisfy (== STrue)
    return $ ElemExpr (EBool True)

operatorSatisfy :: (Operator -> Bool) -> Parser [Token] Operator
operatorSatisfy predicate = do
  token <- next
  let op = getOperator token
  if (predicate op)
    then return op
    else empty

symbolSatisfy :: (Symbol -> Bool) -> Parser [Token] Token
symbolSatisfy = satisfy symbol

isOneOf :: Eq a => [a] -> (a -> Bool)
isOneOf = flip elem
