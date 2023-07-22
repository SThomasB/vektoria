
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
evalOpposite op opposite (Elementary left) (Elementary right) = do
  result <- evaluate (Binary opposite (Elementary left) (Elementary right))
  case result of
    (EBool b) -> return $ EBool (not b)
    _ -> (evalError op left right)

dereference :: Expression -> Runtime Expression
dereference (Reference r) = do
  r' <- getEntity r
  case r' of
    Just (_, thing) -> return thing
    Nothing -> return $ Elementary (EError (r ++ " does not exist"))
dereference (Binary op left right) = do
  left' <- dereference left
  right' <- dereference right
  return $ Binary op left' right'
dereference e = return e




createBindings :: [String]->[Expression] -> Runtime (Maybe [(String, Entity)])
-- Resolve references, and evaluate arguments before binding them to
-- corresponding parameters.
createBindings parameters arguments = do
  let bindings = zip parameters arguments
  maybeResults <- mapM evaluateBinding bindings
  return (sequence maybeResults)
  where
    evaluateBinding (parameter, (Reference ref)) = do
      maybeEntity <- getEntity ref
      case maybeEntity of
        -- The first case where the entity is a Lambda, it should be bound directly
        -- to the parameter; This supports first class functions.
        (Just (_, Lambda parameters arguments)) -> return $ Just (parameter, (Nothing, Lambda parameters arguments))
        -- If the entity is any other expression, we evaluate it before binding it
        -- to the parameter.
        (Just (_, expression)) -> evaluateBinding (parameter, expression)
        (Nothing) -> do
          addError ("Reference error when evaluating arguments")
          return $ Nothing
    evaluateBinding (parameter, argument) = do
      element <- evaluate argument
      case element of
        (EError message) -> do
          addError ("Error evaluating arguments "++message)
          return Nothing
        -- If the argument evaluates to any other element,
        -- we bind it as an Elementary expression.
        element -> return $ Just (parameter, (Nothing, Elementary element))


evaluateCall :: Expression -> [Expression] -> Runtime Element
-- Calls can be made via reference, or directly to a Lambda expression.
-- If the callee is a reference, we resolve the indirection before evaluating
-- the call.
evaluateCall (Reference reference) arguments = do
    entity <- getEntity reference
    case (entity) of
      (Just (_, Lambda parameters expression)) -> evaluateCallable parameters arguments expression
      (Just (_, _)) -> return $ (EError $ "Can only call lambdas")
      _ -> return $ (EError $ "Reference error: "++reference++" does not exist")
evaluateCall (Foreign reference) arguments = do
  entity <- getForeign reference
  case (entity) of
    (Just (_, IOAction action)) -> do
      arguments' <- mapM (fmap Elementary . evaluate) arguments
      result <- liftIO $ action arguments'
      case result of
        (Elementary element) -> return element
        _ -> return $ EError ("Error in foreign function "++reference)
    _ -> return $ EError "No such foreign function"
evaluateCall (Lambda parameters expression) arguments = evaluateCallable parameters arguments expression


evaluateCallable :: [String]->[Expression]->Expression->Runtime Element
-- A call can only be resolved if the number of parameters (arity)
-- matches the number of present arguments in the call expression.
evaluateCallable parameters arguments expression = do
    let arity = (length parameters)
    let argumentsLength = (length arguments)
    if (arity == argumentsLength)
      then do
        bindings <- createBindings parameters arguments
        case bindings of
          Nothing -> return $ EError "Argument error"
          Just validBindings -> do
            -- The call is about to be resolved;
            -- A new scope is created to provide local bindings.
            preCallScope <- get
            put $ newScope preCallScope validBindings
            result <- evaluate expression
            -- the local bindings are abolished by restoring the old scope.
            put preCallScope
            return result
      else return $ EError ("Arity mismatch: "++"expected "++(show arity)++", actual "++(show argumentsLength))


-- Evaluate expressions
evaluate :: Expression -> Runtime Element
evaluate (Reference reference) = do
  reference' <- getEntity reference
  case reference' of
    Just (_, Lambda _ _) -> do
    -- currently not supported to evaluate lambdas like this.
      return $ EError ("Can not evaluate callable ("++reference++")")
    Just (_, expression) -> do
      result <- evaluate expression
      -- To avoid reevaluating expressions
      -- we update the referenced expression with its evaluated form.
      -- Potential optimization to handle the case where the referenced value
      -- is already an Elementary.
      addEntity reference (Nothing, Elementary result)
      return result
    Nothing -> return $ EError ("Reference error: "++reference ++ " does not exist")

evaluate (Call expression args) = evaluateCall expression args
evaluate (Elementary expr) = return expr
evaluate (Tertiary condition left right) = do
  isTrue <- evaluate condition
  case isTrue of
    (EBool True) -> evaluate left
    (EBool False) -> evaluate right
    _ -> return isTrue
-- comparisons
-- And
evaluate (Binary And (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ EBool (left && right)
evaluate (Binary Or (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ EBool (left || right)
-- Equals
evaluate (Binary Equals (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (Elementary (EString left)) (Elementary (EString right))) =
  return $ EBool (left == right)
evaluate (Binary Equals (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ EBool (left == right)
-- NotEquals
evaluate (Binary NotEquals (Elementary left) (Elementary right)) =
  evalOpposite NotEquals Equals (Elementary left) (Elementary right)
-- Greater
evaluate (Binary Greater (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EBool (left > right)
evaluate (Binary Greater (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EBool (left > right)
evaluate (Binary Greater (Elementary (EString left)) (Elementary (EString right))) =
  return $ EBool (left /= right && (isSubstring right left))
-- Less
evaluate (Binary Less (Elementary (EString left)) (Elementary (EString right))) =
  return $ EBool (left /= right && (isSubstring left right))
evaluate (Binary Less (Elementary left) (Elementary right)) =
  evalOpposite Less Greater (Elementary left) (Elementary right)
-- Superset
evaluate (Binary SuperSet (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EBool (left >= right)
evaluate (Binary SuperSet (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EBool (left >= right)
evaluate (Binary SuperSet (Elementary (EString left)) (Elementary (EString right))) =
  return $ EBool (isSubstring right left)
-- Subset
evaluate (Binary SubSet (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EBool (left <= right)
evaluate (Binary SubSet (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EBool (left <= right)
evaluate (Binary SubSet (Elementary (EString left)) (Elementary (EString right))) =
  return $ EBool (isSubstring left right)
-- Multiply
evaluate (Binary Multiply (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EInt (left * right)
evaluate (Binary Multiply (Elementary (EFloat left)) (Elementary (EInt right))) =
  return $ EFloat (left * (fromIntegral right))
evaluate (Binary Multiply (Elementary (EInt left)) (Elementary (EFloat right))) =
  return $ EFloat ((fromIntegral left) * right)
evaluate (Binary Multiply (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EFloat (left * right)
-- Divide
evaluate (Binary Divide (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $  EFloat (left / right)
evaluate (Binary Divide (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EInt (div left right)
-- Minus
evaluate (Binary Minus (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EInt (left - right)
-- Plus
evaluate (Binary Plus (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ EInt (left + right)
evaluate (Binary Plus (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ EFloat (left + right)
evaluate (Binary Plus (Elementary (EString left)) (Elementary (EString right))) =
  return $ EString (left ++ right)
evaluate (Binary Plus (Elementary (EError left)) (Elementary right)) =
  evalError Plus (EError left) right
evaluate (Binary Plus (Elementary left) (Elementary (EError right))) =
  evalError Plus (EError right) left
evaluate (Binary Plus (Elementary (EString left)) (Elementary right)) =
  return $ EString ((left) ++ (showElement right))
evaluate (Binary Plus (Elementary (left)) (Elementary (EString right))) =
  return $ EString ((showElement left) ++ (right))
evaluate (Binary op (Elementary left) (Elementary right)) =
  evalError op left right
evaluate (Binary op left right) = do
    left' <- evaluate left
    right' <- evaluate right
    evaluate (Binary
       op
       (Elementary (left'))
       (Elementary (right')))
