module Vektoria.Lib.Data.Expression where
import Vektoria.Lib.Data.Element
import Data.Unique




data Expression
  = Elementary { element :: Element }
  | Reference { reference :: String }
  | Foreign { reference :: String}
  | Binary { operator:: Operator, left::Expression, right::Expression }
  | Tertiary { condition :: Expression, left :: Expression, right :: Expression}
  | Lambda { closureId :: Maybe Unique,  parameters :: [String],  computation :: Expression }
  | Call { function :: Expression, arguments :: [Expression] }
  | IOAction { action :: ([Expression] ->  IO Expression) }



instance Show Expression where
  show (Elementary element) = "Element: " ++ show element
  show (Binary op expr2 expr3) =
    "Binary Expression: (" ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"
  show (Reference reference) = "Reference "++reference
  show (Call ref args) = "Call "++(show ref)++" "++(show args)
  show (Lambda _ parameters expression) = "Lambda "++(show parameters)++", "++(show expression)
  show (Tertiary _ _ _) = "Tertiary expression"
  show (IOAction _) = "<:IO"
  show (Foreign reference) = "Foreign "++ reference


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
