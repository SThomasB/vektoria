module Vektoria.Lib.Data.Expression where
import Vektoria.Lib.Data.Element



type Parameters = [String]

data Expression
  = Binary Operator Expression Expression
  | ElemExpr Element
  | Ref String
  | Call Expression [Expression]
  | Tertiary Expression Expression Expression
  | Lambda Parameters Expression


instance Show Expression where
  show (ElemExpr element) = "Element: " ++ show element
  show (Binary op expr2 expr3) =
    "Binary Expression: (" ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"
  show (Ref r) = "Reference "++r
  show (Call ref args) = "Call "++(show ref)++" "++(show args)



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
