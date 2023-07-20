module Vektoria.Lib.Data.Element where

data Element
  = EInt Int
  | EString String
  | EFloat Float
  | EBool Bool
  | EVoid
  | EError String
  deriving (Eq)

instance Show Element where
  show (EInt _) = "Integer"
  show (EString _) = "String"
  show (EFloat _) = "Float"
  show (EBool _) = "Bool"
  show (EVoid) = "Void"
  show (EError e) = "Error: "++e


showElement :: Element -> String
showElement (EString v) = v
showElement (EInt v) = show v
showElement (EFloat v) = show v
showElement (EError v) = v
showElement _ = ""
