module Vektoria.Lib.Data.Expression where
import Data.Unique
import Data.List


express :: Element -> Expression
express element = Elementary element

proxy :: Expression -> Element 
proxy expression = EProxy expression
data Element
  = EInt Int
  | EString String
  | EFloat Float
  | EBool Bool
  | EVoid
  | EError String
  | Lambda String [(String, Expression)] [String] Expression
  | EProxy Expression


instance Show Element where
  show (EInt _) = "Integer"
  show (EString _) = "String"
  show (EFloat _) = "Float"
  show (EBool _) = "Bool"
  show (EVoid) = "Void"
  show (EError e) = "Error: "++e
  show (Lambda name close params body) = "Lambda " <> name
  show (EProxy e) = "Proxy: " ++ (show e)

showElement :: Element -> String
showElement (EString v) = v
showElement (EInt v) = show v
showElement (EFloat v) = show v
showElement (EError v) = v
showElement (EBool v) = show v
showElement (EVoid) = "Void"
showElement (Lambda nam _ parameters expression) = "Lambda@"++nam++" "++(show parameters)++", "++(show expression)


data Expression
  = Elementary { element :: Element }
  | Reference { reference :: String }
  | Foreign   { reference :: String }
  | Binary    { operator:: Operator, left::Expression, right::Expression }
  | Ternary  { condition :: Expression, left :: Expression, right :: Expression}
  | Call { function :: Expression, arguments :: [Expression] }
  | IOAction { action :: ([Expression] ->  IO Expression) }
  | Primitive{ func :: ([Expression] -> Expression) }


instance Show Expression where
  show (Elementary element) = (showElement element)
  show (Binary op expr2 expr3) =
    "<:BinExpr " ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3
  show (Reference reference) = "Reference "++reference
  show (Call ref args) = "Call "++(show ref)++" "++(show args)
  show (Ternary _ _ _) = "Ternary expression"
  show (IOAction _) = "<:IO"
  show (Foreign reference) = "Foreign "++ reference

showHL (Elementary element) = showElement element
showHL expr = show expr

showChain (Elementary (EString v)) = "\""++v++"\""
showChain expr = showHL expr

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
  | Tilde
  | And
  | Or
  | NoOp
  deriving (Show, Eq)
