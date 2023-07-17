
module Vektoria.Lib.ParsingGenerics
  ( module Vektoria.Lib.ParsingGenerics
  , module Control.Applicative
  ) where
import Control.Applicative

newtype Parser b a =
  Parser (b -> [(a, b)])

parseWith :: Parser b a -> b -> [(a, b)]
parseWith (Parser parse) = parse

run :: Parser b a -> b -> [([a], b)]
run parser = parseWith $ some parser

instance Functor (Parser b) where
  fmap func (Parser parse) =
    Parser $ \source ->
      case parse source of
        [] -> []
        [(result, remaining)] -> [(func result, remaining)]

instance Applicative (Parser b) where
  pure a = Parser (\source -> [(a, source)])
  (<*>) parserToFunc parser =
    Parser
      (\source ->
         case parseWith parserToFunc source of
           [] -> []
           [(func, remaining)] -> parseWith (fmap func parser) remaining)

instance Monad (Parser b) where
  parserA >>= createParser =
    Parser $ \s ->
      case parseWith parserA s of
        [] -> []
        [(result, remaining)] -> parseWith (createParser result) remaining

instance Alternative (Parser b) where
  empty = Parser (const [])
  Parser (parseA) <|> (Parser parseB) =
    Parser $ \source ->
      case parseA source of
        [] -> parseB source
        [(result, remaining)] -> [(result, remaining)]


next :: Parser [a] a
next = Parser $ \t ->
  case t of
    [] -> []
    (result: remaining) -> [(result, remaining)]

satisfy :: Eq b => (a -> b) -> (b -> Bool) -> Parser [a] a
satisfy transform predicate = do
  item <- next
  if predicate (transform item) then return item else empty


matchItem :: Eq a => a -> Parser [a] a
matchItem target = satisfy id (==target)

matchSequence :: Eq a => [a] -> Parser [a] [a]
matchSequence [] = return []
matchSequence (upNext:rest) = do
  matchItem upNext
  matchSequence rest
  return (upNext:rest)

