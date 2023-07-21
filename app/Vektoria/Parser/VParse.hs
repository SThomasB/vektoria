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
     condition <- parseExpression
     thenBlock <- do block <|> printStatement
     symbolSatisfy (== SElse)
     elseBlock <- do block <|> ifElseStatement <|> printStatement
     return $ IfElse condition thenBlock elseBlock
     <|> do
    symbolSatisfy (== SIf)
    condition <- parseExpression
    thenBlock <- do block <|> printStatement
    return $ IfElse condition thenBlock (Block [])

assignStatement :: Parser [Token] Statement
assignStatement = do
  identifier <- symbolSatisfy (== SIdentifier)
  symbolSatisfy (== SEqual)
  expr <- parseExpression <|> lambda
  let entityName = lexeme identifier
  return (Assign entityName expr)

printStatement :: Parser [Token] Statement
printStatement = do
  symbolSatisfy (== SPrint)
  expr <- parseExpression
  return (Print $ expr)

weakStatement :: Parser [Token] Statement
weakStatement = do
  symbolSatisfy (== SLeftArrow)
  expr <- parseExpression
  return $ Weak expr

timesBracket :: Parser [Token] Expression
timesBracket = do
  left <- factor
  symbolSatisfy (== SLeftBracket)
  right <- parseExpression
  symbolSatisfy (== SRightBracket)
  return $ Binary Multiply left right

functionCall :: Parser [Token] Expression
functionCall = do
  symbolSatisfy (==SLeftParen)
  callee <- lambda <|> parseReference
  args <- some parseExpression
  symbolSatisfy (==SRightParen)
  case callee of
    (Lambda parameters parseExpression) -> return $ Call (Lambda parameters parseExpression) args
    (Reference ref) -> return $ Call (Reference ref) args


lambda :: Parser [Token] Expression
lambda = do
  symbolSatisfy (==SLeftParen)
  parameters <- many $ symbolSatisfy (==SIdentifier)
  let parameters' = map lexeme parameters
  symbolSatisfy (==SComma)
  parseExpression <- parseExpression
  symbolSatisfy (==SRightParen)
  return $ Lambda parameters' parseExpression


parseExpression :: Parser [Token] Expression
parseExpression = do
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
factor = tertiary <|> functionCall <|>literalExpr <|> parenExpr


tertiary :: Parser [Token] Expression
tertiary = do
  symbolSatisfy (==SBar)
  condition <- parseExpression
  symbolSatisfy (==SRightArrow)
  left <- parseExpression
  symbolSatisfy (==SBar)
  right <- parseExpression
  return $ Tertiary condition left right


parenExpr :: Parser [Token] Expression
parenExpr = do
  symbolSatisfy (== SLeftParen)
  expr <- parseExpression
  symbolSatisfy (== SRightParen)
  return expr

parseReference :: Parser [Token] Expression
parseReference = do
  token <- symbolSatisfy (== SIdentifier)
  return $ Reference (lexeme token)

literalExpr :: Parser [Token] Expression
literalExpr = do
  parseReference
  <|> do
    token <- symbolSatisfy (== SString)
    return $ Elementary (EString ((init . tail) $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SInt)
    return $ Elementary (EInt (read $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SFloat)
    return $ Elementary (EFloat (read $ lexeme token))
  <|> do
    token <- symbolSatisfy (== SFalse)
    return $ Elementary (EBool False)
  <|> do
    token <- symbolSatisfy (== STrue)
    return $ Elementary (EBool True)

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
