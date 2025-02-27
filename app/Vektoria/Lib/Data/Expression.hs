module Vektoria.Lib.Data.Expression where
import Vektoria.Lib.Data.Element
import Data.Unique
import Data.List



data Expression
  = Elementary { element :: Element }
  | Member {memberSetName :: String, memberName :: String, fieldNames :: [String]}
  | Set {expressions :: [Expression]}
  | Instance {memberName :: String, fields :: [Expression]}
  | Producer {function :: Expression, produced :: Expression, produce :: Expression}
  | Chain { expressions :: [Expression] }
  | UnChain { expressions :: [Expression] }
  | VirtualChain { from :: Expression, to :: Expression}
  | Dotted { dotted :: Expression}
  | Reference { reference :: String }
  | Foreign { reference :: String}
  | Modified { reference :: String }
  | Binary { operator:: Operator, left::Expression, right::Expression }
  | Tertiary { condition :: Expression, left :: Expression, right :: Expression}
  | Lambda { closure :: [(String, Expression)],  parameters :: [String],  computation :: Expression }
  | Call { function :: Expression, arguments :: [Expression] }
  | IOAction { action :: ([Expression] ->  IO Expression) }
  | Primitive{ func :: ([Expression] -> Expression) }


instance Show Expression where
  show (Elementary element) = "Element: " ++ (showElement element)
  show (Chain []) = "<:Chain []"
  show (Chain xs) = "<:Chain " ++ (concat $ map show xs)
  show (UnChain x) = "<:UnChain " ++show (Chain x)
  show (Binary op expr2 expr3) =
    "<:BinExpr " ++
    show op ++ ", " ++ show expr2 ++ ", " ++ show expr3
  show (Reference reference) = "Reference "++reference
  show (Call ref args) = "Call "++(show ref)++" "++(show args)
  show (Lambda _ parameters expression) = "Lambda "++(show parameters)++", "++(show expression)
  show (Tertiary _ _ _) = "Tertiary expression"
  show (IOAction _) = "<:IO"
  show (Foreign reference) = "Foreign "++ reference
  show (Modified reference) = "<:Modified " ++ reference
  show (Member s m ps) = "<:" ++ s ++ ", " ++ m++" " ++ (show ps)
  show (Set expressions) = "<:" ++ "Set" ++ " " ++(show expressions)
  show (Producer f p n) = "<:Produce" ++ (show f) ++ (show p) ++ (show n)
  show (VirtualChain from to) = (show from) ++ ".."++(show to)

showHL (Elementary element) = showElement element
showHL (Chain expr) = "[" ++ ((concat . (intersperse " ")) (map showChain expr)) ++"]"
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
  | And
  | Or
  | NoOp
  deriving (Show, Eq)
