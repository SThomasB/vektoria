
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
  assignments <- some $ do
    identifier <- lexeme <$> symbolSatisfy (== SIdentifier)
    params <- many $ symbolSatisfy (== SIdentifier)
    symbolSatisfy (== SEqual)
    modifiers <- many $ symbolSatisfy (==SLeftArrow)
    let modifier = if (length modifiers)==0 then [] else [Eager]
    expr <- (setExpression identifier) <|> letInExpression <|> parseExpression <|> lambda
    case map lexeme params of
      ([]) -> return (Assign modifier identifier expr)
      (paramNames) -> return (Assign modifier identifier (Lambda [] paramNames expr))
  case assignments of
    (x:[]) -> return x
    (x:xs) -> return $ Dispatch assignments



setExpression s = do
  symbolSatisfy (==SLeftBrace)
  firstMember <- (memberOf s)
  members <- many $ do
    symbolSatisfy (==SComma)
    member' <- memberOf s
    return $ member'
  symbolSatisfy (==SRightBrace)
  return $ Set (firstMember:members)


memberOf s = do
  ids <- some $ symbolSatisfy (==SIdentifier)
  case map lexeme ids of
    (x:[]) -> return $ Member s x []
    (x:xs) -> return $ Member s x xs

weakStatement :: Parser [Token] Statement
weakStatement = do
  expr <- parseExpression
  return $ Weak expr



functionCall :: Parser [Token] Expression
functionCall = do
  symbolSatisfy (==SLeftParen)
  callee <- foreignCall <|> tertiary <|> letInExpression <|> lambda <|> parseReference <|> functionCall
  args <- some parseExpression
  symbolSatisfy (==SRightParen)
  case callee of
    (Lambda _ parameters body) -> return $ Call (Lambda [] parameters body) args
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
  return $ Call (Lambda [] parameters body) arguments



lambda :: Parser [Token] Expression
lambda = do
  symbolSatisfy (==SLeftParen)
  parameters <- many $ fmap lexeme (symbolSatisfy (==SIdentifier))
  symbolSatisfy (==SComma)
  body <- parseExpression
  symbolSatisfy (==SRightParen)
  return $ Lambda [] parameters body


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
    <|> tertiary
    <|> functionCall
    <|> foreignCall
    <|> dottedExpr
    <|> literalExpr
    <|> parenExpr


tertiary :: Parser [Token] Expression
tertiary = do
  symbolSatisfy (==SQuestion)
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
  reference <- fmap lexeme (symbolSatisfy (== SIdentifier))
  return $ Reference reference

dottedExpr :: Parser [Token] Expression
dottedExpr = do
  symbolSatisfy (==SDot)
  expr <- parseExpression
  return $ Dotted expr

foreignCall :: Parser [Token] Expression
foreignCall = do
    openingParen <- fmap length $ many (symbolSatisfy (==SLeftParen))
    symbolSatisfy (==SAt)
    reference <-   (lexeme <$> (symbolSatisfy (==SIdentifier)))
      <|>((++) <$> (lexeme <$> symbolSatisfy (==SLeftBracket)) <*> (lexeme <$> symbolSatisfy (==SRightBracket)))
      <|>((++) <$> (lexeme <$> symbolSatisfy (==SLeftParen)) <*> (lexeme <$> symbolSatisfy (==SRightParen)))
    expressions <- many parseExpression
    if openingParen /= 0
      then do
        closingParen <- fmap length $ many (symbolSatisfy (==SRightParen))
        if openingParen /= closingParen
         then empty
         else return $ Call (Foreign reference) expressions
    else return $ Call (Foreign reference) expressions

parseChain = do
  token <- symbolSatisfy (==SLeftBracket)
  elements <- some parseExpression
  token <- symbolSatisfy (==SRightBracket)
  return $  Chain elements
literalExpr :: Parser [Token] Expression
literalExpr = do
  virtualChain
  <|> parseChain
  <|> parseReference
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

virtualChain = do
  symbolSatisfy (==SLeftBracket)
  from <- literalExpr
  symbolSatisfy (==SDot)
  symbolSatisfy (==SDot)
  to <- literalExpr
  symbolSatisfy (==SRightBracket)
  return $ VirtualChain from to


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
