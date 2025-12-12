
module Vektoria.Parser.VParse
  ( vektoriaParse
  , run
  ) where


import Data.Char
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Expression
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
    STilde -> Tilde
    _ -> NoOp

-- bin ::= term + expr | expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | int
vektoriaParse :: Parser [Token] Statement
vektoriaParse = statement


statement :: Parser [Token] Statement
statement = reflectStatement <|> assignStatement <|> weakStatement

reflectStatement :: Parser [Token] Statement
reflectStatement = do
    symbolSatisfy (==SDiamond)
    identifier <- symbolSatisfy (==SIdentifier)
    return $ Reflect (Elementary $ EString (lexeme identifier))

assignStatement :: Parser [Token] Statement
assignStatement = do
  symbolSatisfy (==SColon)
  symbolSatisfy (==SColon)
  identifier <- lexeme <$> symbolSatisfy (== SIdentifier)
  params <- many $ symbolSatisfy (== SIdentifier)
  symbolSatisfy (== SEqual)
  modifiers <- many $ symbolSatisfy (==SLeftArrow)
  let modifier = if (length modifiers)==0 then [] else [Eager]
  expr <- parseExpression 
  case map lexeme params of
    ([]) -> return (Assign modifier identifier expr)
    (paramNames) -> return (Assign modifier identifier (express $ Lambda "" [] paramNames expr))






weakStatement :: Parser [Token] Statement
weakStatement = do
  expr <- parseExpression
  return $ Weak expr



functionCall :: Parser [Token] Expression
functionCall = do
  symbolSatisfy (==SLeftParen)
  callee <- foreignCall <|> ternary <|> letInExpression <|> lambda <|> parseReference <|> functionCall
  args <- some parseExpression
  symbolSatisfy (==SRightParen)
  case callee of
    (Elementary lam@(Lambda _ _ parameters body)) -> return $ Call (express $ lam) args
    (Call expression arguments ) -> return $ Call (Call expression arguments) args
    (Reference ref) -> return $ Call (Reference ref) args


letInExpression :: Parser [Token] Expression
letInExpression = do
  symbolSatisfy (==SLet)
  bindings <- some assignStatement
  let parameters = map name bindings
  let arguments = map expression bindings
  symbolSatisfy (==SIn)
  body <- parseExpression
  return $ Call (express $ Lambda "" [] parameters body) arguments



lambda :: Parser [Token] Expression
lambda = do
  symbolSatisfy (==SLeftParen)
  parameters <- many $ fmap lexeme (symbolSatisfy (==SIdentifier))
  symbolSatisfy (==SComma)
  body <- parseExpression
  symbolSatisfy (==SRightParen)
  return $ express $ Lambda "" [] parameters body


parseExpression :: Parser [Token] Expression
parseExpression = do
  lambda
  <|> binaryExpression
    [ Plus
    , Minus
    , Greater
    , Less
    , SubSet
    , SuperSet
    , Equals
    , Tilde
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
factor = letInExpression
    <|> ternary
    <|> functionCall
    <|> foreignCall
    <|> literalExpr
    <|> parenExpr


ternary :: Parser [Token] Expression
ternary = do
  symbolSatisfy (==SQuestion)
  condition <- parseExpression
  symbolSatisfy (==SRightArrow)
  left <- parseExpression
  symbolSatisfy (==SBar)
  right <- parseExpression
  return $ Ternary condition left right


parenExpr :: Parser [Token] Expression
parenExpr = do
  symbolSatisfy (== SLeftParen)
  expr <- parseExpression
  symbolSatisfy (== SRightParen)
  return expr

parseReference :: Parser [Token] Expression
parseReference = do
  reference <- fmap lexeme (symbolSatisfy (== SIdentifier))
  return $ Reference reference


foreignCall :: Parser [Token] Expression
foreignCall = do
    openingParen <- fmap length $ many (symbolSatisfy (==SLeftParen))
    symbolSatisfy (==SAt)
    reference <-   (lexeme <$> (symbolSatisfy (==SIdentifier)))
    expressions <- many parseExpression
    if openingParen /= 0
      then do
        closingParen <- fmap length $ many (symbolSatisfy (==SRightParen))
        if openingParen /= closingParen
         then empty
         else return $ Call (Foreign reference) expressions
    else return $ Call (Foreign reference) expressions

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
