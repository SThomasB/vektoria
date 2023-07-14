module Lib.Parser.Parser (module Lib.Parser.Parser, module Control.Applicative) where
import Control.Applicative
import Data.Char

newtype Parser b a = Parser (b -> [(a, b)])


parseWith :: Parser b a -> b -> [(a, b)]
parseWith (Parser parse) = parse

run :: Parser b a -> b -> [([a], b)]
run parser = parseWith $ some parser


instance Functor (Parser b) where
    fmap func (Parser parse) = Parser $ \source -> case parse source of
        [] -> []
        [(result, remaining)] -> [(func result, remaining)]


instance Applicative (Parser b) where
    pure a = Parser (\source -> [(a, source)])
    (<*>) parserToFunc parser = Parser ( \source -> case parseWith parserToFunc source of
        [] -> []
        [(func, remaining)] -> parseWith (fmap func parser) remaining)


instance Monad (Parser b) where
    parserA >>= createParser  = Parser $ \s -> case parseWith parserA s of
        [] -> []
        [(result, remaining)] -> parseWith (createParser result) remaining

instance Alternative (Parser b) where
    empty = Parser (const [])
    Parser (parseA) <|> (Parser parseB) = Parser $ \source -> case parseA source of
        [] -> parseB source
        [(result, remaining)] -> [(result, remaining)]


char :: Parser String Char
char = Parser $ \source -> case source of
    [] -> []
    (result:remaining) -> [(result, remaining)]

satisfy :: (Char -> Bool) -> Parser String Char
satisfy predicate = do
    sym <- char
    if predicate sym then return sym else empty



glyph :: Char -> Parser String Char
glyph v = satisfy (==v)

glyphs :: String -> Parser String String
glyphs [] = return []
glyphs (next:rest) = do
    glyph next
    glyphs rest
    return (next:rest)



space :: Parser String ()
space = do
    many (satisfy isSpace)
    return ()

ignoreSpace :: Parser String a -> Parser String a
ignoreSpace parser = do
    space
    result <- parser
    space
    return result
