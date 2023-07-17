module Vektoria.Lib.Data.Token where

data Token = Token
  { symbol :: Symbol
  , line :: Int
  , lexeme :: String
  } deriving (Show, Eq)

data Symbol
  = SIdentifier
  | SEqual
  | SEqualEqual
  | SFalse
  | STrue
  | SPrint
  | SPlus
  | SMinus
  | SStar
  | SSlashEqual
  | SSlash
  | SBar
  | SLeftParen
  | SRightParen
  | SDot
  | SComma
  | SColon
  | SLeftBracket
  | SRightBracket
  | SLeftBrace
  | SRightBrace
  | SDiamond
  | SLeft
  | SRight
  | SLeftEqual
  | SRightEqual
  | SAndAnd
  | SBarBar
  | SInt
  | SFloat
  | SString
  deriving (Show, Eq, Enum)

match :: Symbol -> Token -> Bool
match sym token = (symbol token) == sym


