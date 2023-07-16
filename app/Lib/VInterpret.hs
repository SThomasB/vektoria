module Lib.VInterpret
  ( evalExpression
  ) where

import qualified Data.Text as T
import Lib.Data.Token

-- pack converts string to text (not lazy)
isSubstring :: String -> String -> Bool
isSubstring needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

evalError :: Operator -> Element -> Element -> Element
evalError op e1 e2 =
  EError ((show e1) ++ " " ++ (show op) ++ " " ++ (show e2) ++ " is undefined")

evalOpposite :: Operator -> Operator -> Expression -> Expression -> Element
evalOpposite op opposite (ElemExpr left) (ElemExpr right) =
  case (evalExpression (Binary opposite (ElemExpr left) (ElemExpr right))) of
    (EBool b) -> EBool (not b)
    _ -> (evalError op left right)

-- Evaluate expressions
evalExpression :: Expression -> Element
evalExpression (ElemExpr expr) = expr
-- comparisons
-- And
evalExpression (Binary And (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left && right)
evalExpression (Binary Or (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left || right)
-- Equals
evalExpression (Binary Equals (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left == right)
evalExpression (Binary Equals (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left == right)
evalExpression (Binary Equals (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left == right)
evalExpression (Binary Equals (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left == right)
-- NotEquals
evalExpression (Binary NotEquals (ElemExpr left) (ElemExpr right)) =
  evalOpposite NotEquals Equals (ElemExpr left) (ElemExpr right)
-- Greater
evalExpression (Binary Greater (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left > right)
evalExpression (Binary Greater (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left > right)
evalExpression (Binary Greater (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left /= right && (isSubstring right left))
-- Less
evalExpression (Binary Less (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left /= right && (isSubstring left right))
evalExpression (Binary Less (ElemExpr left) (ElemExpr right)) =
  evalOpposite Less Greater (ElemExpr left) (ElemExpr right)
-- Superset
evalExpression (Binary SuperSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left >= right)
evalExpression (Binary SuperSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left >= right)
evalExpression (Binary SuperSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (isSubstring right left)
-- Subset
evalExpression (Binary SubSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left <= right)
evalExpression (Binary SubSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left <= right)
evalExpression (Binary SubSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (isSubstring left right)
-- Multiply
evalExpression (Binary Multiply (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left * right)
-- Divide
evalExpression (Binary Divide (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EFloat (left / right)
evalExpression (Binary Divide (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (div left right)
-- Minus
evalExpression (Binary Minus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left - right)
-- Plus
evalExpression (Binary Plus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left + right)
evalExpression (Binary Plus (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EString (left ++ right)
evalExpression (Binary op (ElemExpr left) (ElemExpr right)) =
  evalError op left right
evalExpression (Binary op left right) =
  evalExpression
    (Binary
       op
       (ElemExpr (evalExpression left))
       (ElemExpr (evalExpression right)))
