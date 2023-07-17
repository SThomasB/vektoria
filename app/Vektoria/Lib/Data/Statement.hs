module Vektoria.Lib.Data.Statement where

data Statement
    =
    Block [Statement]
    | IfElse Expression Statement Statement
    | Assign Entity
    | Print Expression
    | Weak Expression
    deriving Show

data Entity = Entity
  { name :: String
  , thing :: Expression
  }
  deriving Show

data Expression
  = Binary Operator Expression Expression
  | ElemExpr Element
  | Ref String

instance Show Expression where
  show (ElemExpr element) = "Element: " ++ show element
  show (Binary op expr2 expr3) =
    "Binary Expression: (" ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"
  show (Ref r) = "Reference "++r

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

data Element
  = EInt Int
  | EString String
  | EFloat Float
  | EBool Bool
  | EVoid
  | EError String
  deriving (Show, Eq)

