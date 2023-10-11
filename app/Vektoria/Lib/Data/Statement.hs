module Vektoria.Lib.Data.Statement where
import Vektoria.Lib.Data.Expression


data Modifier = Eager deriving Show

data Statement
    =
    Block [Statement]
    | IfElse Expression Statement Statement
    | Assign {modifiers::[Modifier], name::String, expression :: Expression}
    | Print Expression
    | Weak Expression
    deriving Show





