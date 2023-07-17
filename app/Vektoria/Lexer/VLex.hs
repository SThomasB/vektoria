
module Vektoria.Lexer.VLex (vektoriaLex) where
import Vektoria.Lib.Data.Token
import Vektoria.Lib.ParsingGenerics
import Data.Char

type Lexer = Parser String Token
vektoriaLex :: Int -> Lexer
vektoriaLex lineNr = do
    (ignoreSpace $ identifierToken lineNr)
    <|> (ignoreSpace $ printToken lineNr)
    <|> (ignoreSpace $ trueToken lineNr)
    <|> (ignoreSpace $ falseToken lineNr)
    <|> stringToken lineNr
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
    first <- charSatisfy isLower
    remaining <- many $ charSatisfy isAlphaNum
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
rightToken = parseGlyphToken SRight '>'

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

leftEqualToken :: Int -> Lexer
leftEqualToken = parseGlyphsToken SLeftEqual "<="

rightEqualToken :: Int -> Lexer
rightEqualToken = parseGlyphsToken SRightEqual ">="

slashEqualToken :: Int -> Lexer
slashEqualToken = parseGlyphsToken SSlashEqual "/="


barBarToken :: Int -> Lexer
barBarToken = parseGlyphsToken SBarBar "||"


andAndToken :: Int -> Lexer
andAndToken = parseGlyphsToken SAndAnd "&&"

falseToken :: Int -> Lexer
falseToken lineNr = do
    glyphs "False"
    return $ Token SFalse lineNr "False" (EBool False)

trueToken :: Int -> Lexer
trueToken lineNr = do
    glyphs "True"
    return $ Token STrue lineNr "True" (EBool True)

printToken :: Int -> Lexer
printToken lineNr = do
    glyphs "Print"
    return $ Token SPrint lineNr "Print" (EVoid)

stringToken :: Int -> Lexer
stringToken line = do
    first <- glyph '"'
    middle <- some $ charSatisfy (/='"')
    last <- glyph '"'
    return $ Token SString line ((first:middle)++[last]) (EString middle)

intToken :: Int -> Lexer
intToken line = do
    n <- some $ charSatisfy isDigit
    return $ Token SInt line n (EInt (read n))

floatToken :: Int -> Lexer
floatToken line = do
    n <- some $ charSatisfy isDigit
    glyph '.'
    d <- some $ charSatisfy isDigit
    let number = n ++ ('.':d)
    return $ Token SFloat line number (EFloat (read number))


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
