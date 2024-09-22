module Vektoria.Lib.Data.Expression where
import Vektoria.Lib.Data.Element
import Data.Unique
import Data.List



data Expression
  = Elementary { element :: Element }
  | Chain { expressions :: [Expression] }
  | Reference { reference :: String }
  | Foreign { reference :: String}
  | Binary { operator:: Operator, left::Expression, right::Expression }
  | Tertiary { condition :: Expression, left :: Expression, right :: Expression}
  | Lambda { closure :: [(String, Expression)],  parameters :: [String],  computation :: Expression }
  | Call { function :: Expression, arguments :: [Expression] }
  | IOAction { action :: ([Expression] ->  IO Expression) }



instance Show Expression where
  show (Elementary element) = "Element: " ++ (showElement element)
  show (Chain []) = "Chain: []"
  show (Chain xs) = "Chain: " ++ (concat $ map show xs)
  show (Binary op expr2 expr3) =
    "Binary Expression: (" ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3 ++ ")"
  show (Reference reference) = "Reference "++reference
  show (Call ref args) = "Call "++(show ref)++" "++(show args)
  show (Lambda _ parameters expression) = "Lambda "++(show parameters)++", "++(show expression)
  show (Tertiary _ _ _) = "Tertiary expression"
  show (IOAction _) = "<:IO"
  show (Foreign reference) = "Foreign "++ reference

showHL (Elementary element) = showElement element
showHL (Chain expr) = "[" ++ ((concat . (intersperse " ")) (map showHL expr)) ++"]"
showHL expr = show expr

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
