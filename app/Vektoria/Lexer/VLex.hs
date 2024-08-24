
module Vektoria.Lexer.VLex (vektoriaLex) where
import Vektoria.Lib.Data.Token
import Vektoria.Lib.ParsingGenerics
import Data.Char

type Lexer = Parser String Token
vektoriaLex :: Int -> Lexer
vektoriaLex lineNr = do
     (ignoreSpace $ letToken lineNr)
    <|> (ignoreSpace $ inToken lineNr)
    <|> (ignoreSpace $ identifierToken lineNr)
    <|> (ignoreSpace $ leftArrowToken lineNr)
    <|> (ignoreSpace $ atToken lineNr)
    <|> (ignoreSpace $ rightArrowToken lineNr)
    <|> (ignoreSpace $ questionToken lineNr)
    <|> (ignoreSpace $ ifToken lineNr)
    <|> (ignoreSpace $ elseToken lineNr)
    <|> (ignoreSpace $ commaToken lineNr)
    <|> (ignoreSpace $ minusMinusToken lineNr)
    <|> (ignoreSpace $ printToken lineNr)
    <|> (ignoreSpace $ trueToken lineNr)
    <|> (ignoreSpace $ falseToken lineNr)
    <|> (stringToken lineNr)
    <|> (ignoreSpace $ leftBracketToken lineNr)
    <|> (ignoreSpace $ rightBracketToken lineNr)
    <|> (ignoreSpace $ floatToken lineNr)
    <|> (ignoreSpace $ intToken lineNr)
    <|> (ignoreSpace $ barBarToken lineNr)
    <|> (ignoreSpace $ andAndToken lineNr)
    <|> (ignoreSpace $ slashEqualToken lineNr)
    <|> (ignoreSpace $ leftEqualToken lineNr)
    <|> (ignoreSpace $ rightEqualToken lineNr)
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
    first <- (charSatisfy isLower) <|> (glyph '_')
    remaining <- many $ (charSatisfy isAlphaNum) <|> (glyph '_')
    return $ Token SIdentifier line (first:remaining)


parseGlyphToken :: Symbol -> Char -> Int -> Lexer
parseGlyphToken sym thisGlyph line = do
    glyph thisGlyph
    return $ Token sym line [thisGlyph]

atToken :: Int -> Lexer
atToken = parseGlyphToken SAt '@'

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

questionToken :: Int -> Lexer
questionToken = parseGlyphToken SQuestion '?'

leftToken :: Int -> Lexer
leftToken = parseGlyphToken SLeft '<'

rightToken :: Int -> Lexer
rightToken = parseGlyphToken SRight '>'

starToken :: Int -> Lexer
starToken = parseGlyphToken SStar '*'

slashToken :: Int -> Lexer
slashToken = parseGlyphToken SSlash '/'

barToken :: Int -> Lexer
barToken = parseGlyphToken SBar '|'


equalToken :: Int -> Lexer
equalToken = parseGlyphToken SEqual '='

commaToken :: Int -> Lexer
commaToken = parseGlyphToken SComma ','

minusToken :: Int -> Lexer
minusToken = parseGlyphToken SMinus '-'


plusToken :: Int -> Lexer
plusToken = parseGlyphToken SPlus '+'


parseGlyphsToken :: Symbol -> String -> Int -> Lexer
parseGlyphsToken sym theseGlyphs line = do
    glyphs theseGlyphs
    return $ Token sym line theseGlyphs

diamondToken :: Int -> Lexer
diamondToken = parseGlyphsToken SDiamond "<>"


equalEqualToken :: Int -> Lexer
equalEqualToken = parseGlyphsToken SEqualEqual "=="

leftEqualToken :: Int -> Lexer
leftEqualToken = parseGlyphsToken SLeftEqual "<="

rightEqualToken :: Int -> Lexer
rightEqualToken = parseGlyphsToken SRightEqual ">="

slashEqualToken :: Int -> Lexer
slashEqualToken = parseGlyphsToken SSlashEqual "/="

leftArrowToken :: Int -> Lexer
leftArrowToken = parseGlyphsToken SLeftArrow "<-"

rightArrowToken :: Int -> Lexer
rightArrowToken = parseGlyphsToken SRightArrow "->"
barBarToken :: Int -> Lexer
barBarToken = parseGlyphsToken SBarBar "||"


andAndToken :: Int -> Lexer
andAndToken = parseGlyphsToken SAndAnd "&&"

minusMinusToken :: Int -> Lexer
minusMinusToken = parseGlyphsToken SMinusMinus "--"
falseToken :: Int -> Lexer
falseToken lineNr = do
    glyphs "False"
    return $ Token SFalse lineNr "False"

trueToken :: Int -> Lexer
trueToken lineNr = do
    glyphs "True"
    return $ Token STrue lineNr "True"

printToken :: Int -> Lexer
printToken lineNr = do
    glyphs "Print"
    return $ Token SPrint lineNr "Print"


ifToken :: Int -> Lexer
ifToken lineNr = do
    glyphs "If"
    return $ Token SIf lineNr "If"

elseToken :: Int -> Lexer
elseToken lineNr = do
    glyphs "Else"
    return $ Token SElse lineNr "Else"

letToken :: Int -> Lexer
letToken lineNr = do
    glyphs "let"
    some $ charSatisfy isSpace
    return $ Token SLet lineNr "let"

inToken :: Int -> Lexer
inToken lineNr = do
    glyphs "in"
    some $ charSatisfy isSpace
    return $ Token SIn lineNr "in"

stringToken :: Int -> Lexer
stringToken line = do
    space
    first <- glyph '"'
    middle <- many $ charSatisfy (/='"')
    last <- glyph '"'
    space
    return $ Token SString line ((first:middle)++[last])

intToken :: Int -> Lexer
intToken line = do
    n <- some $ charSatisfy isDigit
    return $ Token SInt line n


floatToken :: Int -> Lexer
floatToken line = do
    n <- some $ charSatisfy isDigit
    glyph '.'
    d <- some $ charSatisfy isDigit
    let number = n ++ ('.':d)
    return $ Token SFloat line number

char :: Parser String Char
char = next

charSatisfy :: (Char -> Bool) -> Parser String Char
charSatisfy = satisfy id

glyph :: Char -> Parser String Char
glyph = matchItem

glyphs :: String -> Parser String String
glyphs = matchSequence

space :: Parser String ()
space = do
    many (charSatisfy isSpace)
    return ()

ignoreSpace :: Parser String a -> Parser String a
ignoreSpace parser = do
    space
    result <- parser
    space
    return result
