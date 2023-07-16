module Lib.Data.Token where

data Symbol
  = SIdentifier
  | SEqual
  | SEqualEqual
  | SFalse
  | STrue
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

data Token = Token
  { symbol :: Symbol
  , line :: Int
  , lexeme :: String
  , element :: Element
  } deriving (Show, Eq)

match :: Symbol -> Token -> Bool
match sym token = (symbol token) == sym

data Element
  = EInt Int
  | EString String
  | EFloat Float
  | EBool Bool
  | EVoid
  | EError String
  deriving (Show, Eq)

data Expression
  = Binary Operator Expression Expression
  | ElemExpr Element

instance Show Expression where
  show (ElemExpr element) = "Element: " ++ show element
  show (Binary op expr2 expr3) =
    "Binary Expression: (" ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"

data Operator
  = Plus
  | Minus
  | Multiply
  | Divide
  | Equals
  | NotEquals
  | Less
  | Greater
  | SubSet
  | SuperSet
  | And
  | Or
  | NoOp
  deriving (Show, Eq)

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
