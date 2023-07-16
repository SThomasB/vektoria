module Lib.VInterpret
  ( evalExpr, interpretAssign, VState, initState)
  where

import qualified Data.Text as T
import Lib.Data.Token
import qualified Data.HashMap.Strict as HashMap

type VState = HashMap.HashMap String Entity

initState :: VState
initState = HashMap.empty

interpretAssign :: VState -> Statement -> VState
interpretAssign state (Assign e) = HashMap.insert (name e) e state


-- pack converts string to text (not lazy)
isSubstring :: String -> String -> Bool
isSubstring needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

evalError :: Operator -> Element -> Element -> Element
evalError op e1 e2 =
  EError ((show e1) ++ " " ++ (show op) ++ " " ++ (show e2) ++ " is undefined")

evalOpposite :: Operator -> Operator -> Expression -> Expression -> Element
evalOpposite op opposite (ElemExpr left) (ElemExpr right) =
  case (evalExpr (Binary opposite (ElemExpr left) (ElemExpr right))) of
    (EBool b) -> EBool (not b)
    _ -> (evalError op left right)

-- Evaluate expressions
evalExpr :: Expression -> Element
evalExpr (ElemExpr expr) = expr
-- comparisons
-- And
evalExpr (Binary And (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left && right)
evalExpr (Binary Or (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left || right)
-- Equals
evalExpr (Binary Equals (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left == right)
evalExpr (Binary Equals (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left == right)
evalExpr (Binary Equals (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left == right)
evalExpr (Binary Equals (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  EBool (left == right)
-- NotEquals
evalExpr (Binary NotEquals (ElemExpr left) (ElemExpr right)) =
  evalOpposite NotEquals Equals (ElemExpr left) (ElemExpr right)
-- Greater
evalExpr (Binary Greater (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left > right)
evalExpr (Binary Greater (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left > right)
evalExpr (Binary Greater (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left /= right && (isSubstring right left))
-- Less
evalExpr (Binary Less (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (left /= right && (isSubstring left right))
evalExpr (Binary Less (ElemExpr left) (ElemExpr right)) =
  evalOpposite Less Greater (ElemExpr left) (ElemExpr right)
-- Superset
evalExpr (Binary SuperSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left >= right)
evalExpr (Binary SuperSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left >= right)
evalExpr (Binary SuperSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (isSubstring right left)
-- Subset
evalExpr (Binary SubSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EBool (left <= right)
evalExpr (Binary SubSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EBool (left <= right)
evalExpr (Binary SubSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EBool (isSubstring left right)
-- Multiply
evalExpr (Binary Multiply (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left * right)
-- Divide
evalExpr (Binary Divide (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  EFloat (left / right)
evalExpr (Binary Divide (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (div left right)
-- Minus
evalExpr (Binary Minus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left - right)
-- Plus
evalExpr (Binary Plus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  EInt (left + right)
evalExpr (Binary Plus (ElemExpr (EString left)) (ElemExpr (EString right))) =
  EString (left ++ right)
evalExpr (Binary op (ElemExpr left) (ElemExpr right)) =
  evalError op left right
evalExpr (Binary op left right) =
  evalExpr
    (Binary
       op
       (ElemExpr (evalExpr left))
       (ElemExpr (evalExpr right)))
