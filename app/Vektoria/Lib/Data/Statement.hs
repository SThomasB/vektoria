module Vektoria.Lib.Data.Statement where
import Vektoria.Lib.Data.Expression


data Modifier = Eager deriving Show

data Statement
    =
    Block [Statement]
    | Assign {modifiers::[Modifier], name::String, expression :: Expression}
    | Weak Expression
    | Reflect Expression
    deriving Show





