module Vektoria.Lib.Data.Statement where
import Vektoria.Lib.Data.Expression
data Statement
    =
    Block [Statement]
    | IfElse Expression Statement Statement
    | Assign String Expression
    | Print Expression
    | Weak Expression
    deriving Show





