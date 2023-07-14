module Lib.VLex (vektoriaLex, vektoriaParse, run, evalExpression) where
import Lib.Parser.Parser
import Data.Char
import Lib.Data.Token

-- Expression: Literal | Unary | Binary | Grouping ;
-- Literal: VInt | VFloat | VString ;
-- Grouping: VLeftParen Expression VRightParen ;
-- Unary: VMinus;
-- Binary: Expression Operator Expression;
-- Operator: VEqualEqual | VLeft | VRight | VLeftEqual
--          | VRight | VRightEqual | VMinus | VPlus | VStar | VSlash



data Operator = Plus
               | Minus
               | Multiply
               | Divide
               | Equals
               | NoOp
               deriving (Show, Eq)


data Expression = Binary Operator Expression Expression
                 | ElemExpr Element

instance Show Expression where
  show (ElemExpr element) = "Element: " ++ show element
  show (Binary op expr2 expr3) = "Binary Expression: (" ++ show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"



evalExpression :: Expression -> Element
evalExpression (ElemExpr expr) = expr
evalExpression (Binary Multiply (ElemExpr (EInt left)) (ElemExpr (EInt right))) = EInt (left*right)
evalExpression (Binary Divide (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) = EFloat (left/right)
evalExpression (Binary Divide (ElemExpr (EInt left)) (ElemExpr (EInt right))) = EInt (div left right)
evalExpression (Binary Minus (ElemExpr (EInt left)) (ElemExpr (EInt right))) = EInt (left-right)
evalExpression (Binary Plus (ElemExpr (EInt left)) (ElemExpr (EInt right))) = EInt (left+right)
evalExpression (Binary Plus (ElemExpr (EString left)) (ElemExpr (EString right))) = EString (left ++ right)
evalExpression (Binary op left right) = evalExpression (Binary op (ElemExpr (evalExpression left)) (ElemExpr (evalExpression right)))


getOperator :: Token -> Operator
getOperator token = case (symbol token) of
    SPlus    -> Plus
    SMinus   -> Minus
    SEqual  -> Equals
    SStar    -> Multiply
    SSlash   -> Divide
    _       -> NoOp


operator :: Parser [Token] Operator
operator = do
    token <- next
    let op = getOperator token
    return op



isLiteral :: Token -> Bool
isLiteral token = (element token)/=EVoid


-- bin ::= term + expr | expr | term
-- term ::= factor * term | factor
-- factor ::= (expr) | int

vektoriaParse :: Parser [Token] Expression
vektoriaParse = do
    expression


matchSymbol :: Symbol -> Parser [Token] Token
matchSymbol sym = do
    token <- next
    if (match sym token) then return token else empty

matchOneOf :: [Symbol] -> Parser [Token] Token
matchOneOf syms = do
    token <- next
    if ((symbol token) `elem` syms) then return token else empty

binaryExpression :: [Symbol] -> Parser [Token] Expression -> Parser [Token] Expression
binaryExpression syms operand = do
  left <- operand
  rest <- many $ do
    token <- matchOneOf syms
    let op = getOperator token
    right <- operand
    return (op, right)
  return $ foldl (\acc (op, expr) -> Binary op acc expr) left rest


expression :: Parser [Token] Expression
expression = binaryExpression [SPlus, SMinus] term

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
    if (isLiteral token) then return (ElemExpr (element token)) else empty


parseOp :: Operator -> Parser [Token] Operator
parseOp thisOp = do
    n <- next
    let op = getOperator n
    if (thisOp == op) then return op else empty



next :: Parser [Token] Token
next = Parser $ \tokens -> case tokens of
    [] -> []
    (result:remaining) -> [(result, remaining)]


type Lexer = Parser String Token
vektoriaLex :: Int -> Lexer
vektoriaLex lineNr = do
    (ignoreSpace $ identifierToken lineNr)
    <|> stringToken lineNr
    <|> (ignoreSpace $ floatToken lineNr)
    <|> (ignoreSpace $ intToken lineNr)
    <|> (ignoreSpace $ equalEqualToken lineNr)
    <|> (ignoreSpace $ equalToken lineNr)
    <|> (ignoreSpace $ minusToken lineNr)
    <|> (ignoreSpace $ plusToken lineNr)
    <|> (ignoreSpace $ diamondToken lineNr)
    <|> (ignoreSpace $ leftToken lineNr)
    <|> (ignoreSpace $ rightToken lineNr)
    <|> (ignoreSpace $ starToken lineNr)
    <|> (ignoreSpace $ slashToken lineNr)
    <|> (ignoreSpace $ leftBraceToken lineNr)
    <|> (ignoreSpace $ rightBraceToken lineNr)
    <|> (ignoreSpace $ leftBracketToken lineNr)
    <|> (ignoreSpace $ rightBracketToken lineNr)
    <|> (ignoreSpace $ leftParenToken lineNr)
    <|> (ignoreSpace $ rightParenToken lineNr)
    <|> (ignoreSpace $ barToken lineNr)


identifierToken :: Int -> Lexer
identifierToken line = do
    first <- satisfy isLower
    remaining <- many $ satisfy isAlphaNum
    return $ Token SIdentifier line (first:remaining) EVoid


parseGlyphToken :: Symbol -> Char -> Int -> Lexer
parseGlyphToken sym thisGlyph line = do
    glyph thisGlyph
    return $ Token sym line [thisGlyph] EVoid

leftParenToken :: Int -> Lexer
leftParenToken = parseGlyphToken SLeftParen '('
rightParenToken :: Int -> Lexer
rightParenToken = parseGlyphToken SRightParen ')'


leftBracketToken :: Int -> Lexer
leftBracketToken = parseGlyphToken SLeftBracket '['

rightBracketToken :: Int -> Lexer
rightBracketToken = parseGlyphToken SRightBracket ']'

leftBraceToken :: Int -> Lexer
leftBraceToken = parseGlyphToken SLeftBrace '{'


rightBraceToken :: Int -> Lexer
rightBraceToken = parseGlyphToken SRightBrace '}'


leftToken :: Int -> Lexer
leftToken = parseGlyphToken SLeft '<'

rightToken :: Int -> Lexer
rightToken = parseGlyphToken SRight '<'

starToken :: Int -> Lexer
starToken = parseGlyphToken SStar '*'

slashToken :: Int -> Lexer
slashToken = parseGlyphToken SSlash '/'

barToken :: Int -> Lexer
barToken = parseGlyphToken SBar '|'


equalToken :: Int -> Lexer
equalToken = parseGlyphToken SEqual '='

minusToken :: Int -> Lexer
minusToken = parseGlyphToken SMinus '-'


plusToken :: Int -> Lexer
plusToken = parseGlyphToken SPlus '+'


parseGlyphsToken :: Symbol -> String -> Int -> Lexer
parseGlyphsToken sym theseGlyphs line = do
    glyphs theseGlyphs
    return $ Token sym line theseGlyphs EVoid

diamondToken :: Int -> Lexer
diamondToken = parseGlyphsToken SDiamond "<>"


equalEqualToken :: Int -> Lexer
equalEqualToken = parseGlyphsToken SEqualEqual "=="

stringToken :: Int -> Lexer
stringToken line = do
    first <- glyph '"'
    middle <- some $ satisfy (/='"')
    last <- glyph '"'
    return $ Token SString line ((first:middle)++[last]) (EString middle)

intToken :: Int -> Lexer
intToken line = do
    n <- some $ satisfy isDigit
    return $ Token SInt line n (EInt (read n))

floatToken :: Int -> Lexer
floatToken line = do
    n <- some $ satisfy isDigit
    glyph '.'
    d <- some $ satisfy isDigit
    let number = n ++ ('.':d)
    return $ Token SFloat line number (EFloat (read number))



