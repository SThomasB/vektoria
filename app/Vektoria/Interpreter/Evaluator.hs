
module Vektoria.Interpreter.Evaluator (evaluate) where
import Vektoria.Lib.Data.Statement
import Vektoria.Interpreter.Runtime
import qualified Data.Text as T


-- pack converts string to text (not lazy)
isSubstring :: String -> String -> Bool
isSubstring needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)


evalError :: Operator -> Element -> Element -> Runtime Element
evalError op e1 e2 =
  return $ EError ((show e1) ++ " " ++ (show op) ++ " " ++ (show e2) ++ " is undefined")


evalOpposite :: Operator -> Operator -> Expression -> Expression -> Runtime Element
evalOpposite op opposite (ElemExpr left) (ElemExpr right) = do
  result <- evaluate (Binary opposite (ElemExpr left) (ElemExpr right))
  case result of
    (EBool b) -> return $ EBool (not b)
    _ -> (evalError op left right)

dereference :: Expression -> Runtime Expression
dereference (Ref r) = do
  r' <- getEntity r
  case r' of
    Just (Entity name thing) -> return thing
    Just (Callable name thing) -> return thing
    Nothing -> return $ ElemExpr (EError (r ++ " does not exist"))
dereference (Binary op left right) = do
  left' <- dereference left
  right' <- dereference right
  return $ Binary op left' right'
dereference e = return e


-- Evaluate expressions
evaluate :: Expression -> Runtime Element
evaluate (Call (Ref r) args) = return $ EError "Can not evaluate"
evaluate (ElemExpr expr) = return expr
evaluate (Ref r) = return $ EError r

-- comparisons
-- And
evaluate (Binary And (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  return $ EBool (left && right)
evaluate (Binary Or (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  return $ EBool (left || right)
-- Equals
evaluate (Binary Equals (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (ElemExpr (EBool left)) (ElemExpr (EBool right))) =
  return $ EBool (left == right)
-- NotEquals
evaluate (Binary NotEquals (ElemExpr left) (ElemExpr right)) =
  evalOpposite NotEquals Equals (ElemExpr left) (ElemExpr right)
-- Greater
evaluate (Binary Greater (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EBool (left > right)
evaluate (Binary Greater (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EBool (left > right)
evaluate (Binary Greater (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EBool (left /= right && (isSubstring right left))
-- Less
evaluate (Binary Less (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EBool (left /= right && (isSubstring left right))
evaluate (Binary Less (ElemExpr left) (ElemExpr right)) =
  evalOpposite Less Greater (ElemExpr left) (ElemExpr right)
-- Superset
evaluate (Binary SuperSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EBool (left >= right)
evaluate (Binary SuperSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EBool (left >= right)
evaluate (Binary SuperSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EBool (isSubstring right left)
-- Subset
evaluate (Binary SubSet (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EBool (left <= right)
evaluate (Binary SubSet (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EBool (left <= right)
evaluate (Binary SubSet (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EBool (isSubstring left right)
-- Multiply
evaluate (Binary Multiply (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EInt (left * right)
evaluate (Binary Multiply (ElemExpr (EFloat left)) (ElemExpr (EInt right))) =
  return $ EFloat (left * (fromIntegral right))
evaluate (Binary Multiply (ElemExpr (EInt left)) (ElemExpr (EFloat right))) =
  return $ EFloat ((fromIntegral left) * right)
evaluate (Binary Multiply (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EFloat (left * right)
-- Divide
evaluate (Binary Divide (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $  EFloat (left / right)
evaluate (Binary Divide (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EInt (div left right)
-- Minus
evaluate (Binary Minus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EInt (left - right)
-- Plus
evaluate (Binary Plus (ElemExpr (EInt left)) (ElemExpr (EInt right))) =
  return $ EInt (left + right)
evaluate (Binary Plus (ElemExpr (EFloat left)) (ElemExpr (EFloat right))) =
  return $ EFloat (left + right)
evaluate (Binary Plus (ElemExpr (EString left)) (ElemExpr (EString right))) =
  return $ EString (left ++ right)
evaluate (Binary Plus (ElemExpr left) (ElemExpr right)) =
  return $ EString ((showElement left) ++ (showElement right))
evaluate (Binary op (ElemExpr left) (ElemExpr right)) =
  evalError op left right
evaluate (Binary op left right) = do
    left' <- evaluate left
    right' <- evaluate right
    evaluate (Binary
       op
       (ElemExpr (left'))
       (ElemExpr (right')))
