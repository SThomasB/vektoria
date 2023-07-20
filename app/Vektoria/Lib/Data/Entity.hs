
module Vektoria.Lib.Data.Entity where
import Vektoria.Lib.Data.Expression

data Entity = Computable { thing :: Expression }
  | Callable { parameters :: [String], thing :: Expression }
  deriving Show


addCallable :: Entity
addCallable = Callable ["a", "b", "c"] (Binary Plus (Ref "a") (Binary Plus (Ref "b") (Ref "c")))


testCallables :: [(String, Entity)]
testCallables = [("add", addCallable)]
