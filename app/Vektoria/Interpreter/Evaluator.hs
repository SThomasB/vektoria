
module Vektoria.Interpreter.Evaluator (evaluate, dereference) where
import Vektoria.Lib.Data.Expression
import Vektoria.Interpreter.Runtime
import Vektoria.Lib.Data.Entity
import Vektoria.Lib.Data.Element
import qualified Data.Text as T


-- pack converts string to text (not lazy)
isSubstring :: String -> String -> Bool
isSubstring needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)


evalError :: Operator -> Element -> Element -> Runtime Element
evalError op e1 e2 =
  return $ EError ("("++(show e1) ++ ") " ++ (show op) ++ " (" ++ (show e2) ++") "++ " is undefined")


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
    Just (Computable thing) -> return thing
    Just (Callable bindings thing) -> return thing
    Nothing -> return $ ElemExpr (EError (r ++ " does not exist"))
dereference (Binary op left right) = do
  left' <- dereference left
  right' <- dereference right
  return $ Binary op left' right'
dereference e = return e




createBindings :: [String]->[Expression] -> Runtime (Maybe [(String, Entity)])
createBindings parameters arguments = do
  let bindings = zip parameters arguments
  maybeResults <- mapM evaluateBinding bindings
  return (sequence maybeResults)
  where
    evaluateBinding (parameter, (Ref ref)) = do
      maybeEntity <- getEntity ref
      case maybeEntity of
        (Just (Callable parameters arguments)) -> return $ Just (parameter, Callable parameters arguments)
        (Just (Computable expression)) -> evaluateBinding (parameter, expression)
        (Nothing) -> do
          addError ("Reference error when evaluating arguments")
          return $ Nothing
    evaluateBinding (parameter, argument) = do
      element <- evaluate argument
      case element of
        (EError message) -> do
          addError ("Error evaluating arguments "++message)
          return Nothing
        value -> return $ Just (parameter, Computable (ElemExpr value))


evaluateCall :: Expression -> [Expression] -> Runtime Element
evaluateCall (Ref reference) arguments = do
    entity <- getEntity reference
    case (entity) of
      (Just (Callable parameters expression)) -> evaluateCallable parameters arguments expression
      _ -> return $ (EError $ "Reference error: "++reference++" does not exist")

evaluateCall (Lambda parameters expression) arguments = evaluateCallable parameters arguments expression


evaluateCallable :: [String]->[Expression]->Expression->Runtime Element
evaluateCallable parameters arguments expression = do
    let arity = (length parameters)
    let argumentsLength = (length arguments)
    if (arity == argumentsLength)
      then do
        bindings <- createBindings parameters arguments
        case bindings of
          Nothing -> return $ EError "Argument error"
          Just validBindings -> do
            preCallScope <- get
            put $ newScope preCallScope validBindings
            result <- evaluate expression
            put preCallScope
            return result
      else return $ EError ("Arity mismatch: "++"expected "++(show arity)++", actual "++(show argumentsLength))


-- Evaluate expressions
evaluate :: Expression -> Runtime Element
evaluate (Ref r) = do
  r' <- getEntity r
  case r' of
    Just (Computable thing) -> do
      result <- evaluate thing
      addEntity r (Computable (ElemExpr result))
      return result
    Just (Callable _ _) -> do
      return $ EError ("Can not evaluate callable ("++r++")")
    Nothing -> return $ EError ("Reference error: "++r ++ " does not exist")

evaluate (Call expression args) = evaluateCall expression args
evaluate (ElemExpr expr) = return expr
evaluate (Tertiary condition left right) = do
  isTrue <- evaluate condition
  case isTrue of
    (EBool True) -> evaluate left
    (EBool False) -> evaluate right
    _ -> return isTrue
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
evaluate (Binary Plus (ElemExpr (EError left)) (ElemExpr right)) =
  evalError Plus (EError left) right
evaluate (Binary Plus (ElemExpr left) (ElemExpr (EError right))) =
  evalError Plus (EError right) left
evaluate (Binary Plus (ElemExpr (EString left)) (ElemExpr right)) =
  return $ EString ((left) ++ (showElement right))
evaluate (Binary Plus (ElemExpr (left)) (ElemExpr (EString right))) =
  return $ EString ((showElement left) ++ (right))
evaluate (Binary op (ElemExpr left) (ElemExpr right)) =
  evalError op left right
evaluate (Binary op left right) = do
    left' <- evaluate left
    right' <- evaluate right
    evaluate (Binary
       op
       (ElemExpr (left'))
       (ElemExpr (right')))
