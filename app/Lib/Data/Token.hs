module Lib.Data.Token where

data Symbol
  = SIdentifier
  | SEqual
  | SEqualEqual
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

data Element
  = EInt Int
  | EString String
  | EFloat Float
  | EBool Bool
  | EVoid
  | EError String
  deriving (Show, Eq)

data Token = Token
  { symbol :: Symbol
  , line :: Int
  , lexeme :: String
  , element :: Element
  } deriving (Show, Eq)

match :: Symbol -> Token -> Bool
match sym token = (symbol token) == sym
