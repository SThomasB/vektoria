
module Vektoria.Interpreter.Evaluator (evaluate, dereference) where
import Vektoria.Lib.Data.Expression
import Vektoria.Interpreter.Runtime
import Vektoria.Lib.Data.Entity
import Vektoria.Lib.Data.Element
import Control.Monad
import Data.Unique
import qualified Data.Text as T

-- pack converts string to text (not lazy)
isSubstring :: String -> String -> Bool
isSubstring needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

evalError :: Operator -> Element -> Element -> Runtime Expression
evalError op e1 e2 =
  return $ Elementary (EError ("("++(show e1) ++ ") " ++ (show op) ++ " (" ++ (show e2) ++") "++ " is undefined"))

evalOpposite :: Operator -> Operator -> Expression -> Expression -> Runtime Expression
evalOpposite op opposite (Elementary left) (Elementary right) = do
  result <- evaluate (Binary opposite (Elementary left) (Elementary right))
  case result of
    (Elementary (EBool b)) -> return $ Elementary (EBool (not b))
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
        (Just (_, Lambda closure parameters arguments)) -> return $ Just (parameter, (Nothing, Lambda closure parameters arguments))
        -- If the entity is any other expression, we evaluate it before binding it
        -- to the parameter.
        (Just (_, expression)) -> evaluateBinding (parameter, expression)
        (Nothing) -> do
          addError ("Reference error when evaluating arguments")
          return $ Nothing
    evaluateBinding (parameter, argument) = do
      expression <- evaluate argument
      case expression of
        Elementary (EError message) -> do
          addError ("Error evaluating arguments "++message)
          return Nothing
        -- If the argument evaluates to any other element,
        -- we bind it as an Elementary expression.
        expression -> return $ Just (parameter, (Nothing, expression))


evaluateCall :: Expression -> [Expression] -> Runtime Expression
-- Calls can be made via reference, or directly to a Lambda expression.
-- If the callee is a reference, we resolve the indirection before evaluating
-- the call.
evaluateCall (Reference reference) arguments = do
    entity <- evaluate (Reference reference)
    case (entity) of
      (Lambda closure parameters expression) -> evaluateCallable closure parameters arguments expression
      _ -> do
        return $ Elementary (EError "Can only call lambdas")
evaluateCall (Foreign reference) arguments = do
  entity <- evaluate (Foreign reference)
  case (entity) of
    (IOAction action) -> do
      case arguments of
        [] -> liftIO $ action []
        _ -> do
          arguments' <- forM arguments  $ \arg -> do
                argument' <- evaluate arg
                return argument'
          result <- liftIO $ action arguments'
          --returns <- forM arguments (\arg -> do
            --argument' <- evaluate arg
            --liftIO $ action [argument'])
          return $ result
          --liftIO $ action arguments'
    _ -> return $ Elementary (EError "No such foreign function")

evaluateCall (Lambda closure parameters expression) arguments = evaluateCallable closure parameters arguments expression
evaluateCall (Call expression arguments) outerArguments = do
  callee <- evaluateCall expression arguments
  case callee of
    (Lambda closure parameters expression) -> evaluateCallable closure parameters outerArguments expression
    _ -> return $ Elementary (EError "Callees must evaluate to lambdas")
  evaluateCall callee outerArguments
evaluateCall expression arguments = do
  return $ Elementary (EError "Cant handle this case")

resolve :: [String] -> Expression -> Runtime Expression

resolve protected (Reference reference) = do
  if reference `elem` protected
    then return $ Reference reference
    else entity reference


resolve  protected (Call callee arguments) = do
  callee' <- resolve protected callee
  arguments' <- mapM (resolve protected) arguments
  return $ Call callee arguments'


resolve  protected (Tertiary condition left right) = do
  condition' <- resolve protected condition
  left' <- resolve protected left
  right' <- resolve protected right
  return $ Tertiary condition left right


resolve  protected (Binary op left right) = do
  left' <- resolve protected left
  right' <- resolve protected right
  return $ Binary op left' right'

resolve protected (Lambda closureId parameters expression) = do
  resolve parameters expression

resolve  _ (expression) = return expression


entity :: String -> Runtime Expression
entity reference = do
  maybeEntity <- getEntity reference
  case maybeEntity of
    Just (_, thing) -> return thing
    Nothing -> return $ Elementary (EError (reference ++ " does not exist"))



evaluateCallable :: [(String, Expression)]->[String]->[Expression]->Expression->Runtime Expression
-- A call can only be resolved if the number of parameters (arity)
-- matches the number of present arguments in the call expression.
evaluateCallable closure [p] [(Chain arguments)] expression = do
   Chain <$> sequence (map (\x -> evaluateCallable closure [p] [x] expression) arguments)

evaluateCallable closure parameters arguments expression = do
    let arity = (length parameters)
    let argumentsLength = (length arguments)
    if (arity == argumentsLength)
      then do
        let (closureParams,closureArgs) = unzip closure
        bindings <- createBindings (closureParams++parameters) (closureArgs ++ arguments)
        case bindings of
          Nothing -> return $ Elementary (EError "Argument error")
          Just validBindings -> do
            -- The call is about to be resolved;
            -- A new scope is created to provide local bindings.
            preCallScope <- get
            put $ newScope preCallScope validBindings
            result <- evaluate expression
            result' <- do
              case result of
                (Lambda maybeId parameters expression) -> do
                  expression' <- resolve parameters expression
                  return $ Lambda maybeId parameters expression'
                _ -> return result
            -- the local bindings are abolished by restoring the old scope.
            put preCallScope
            return result'
      -- partial application
    else return $ Lambda (zip (take argumentsLength parameters) (arguments)) (drop argumentsLength parameters) expression
-- Evaluate expressions
evaluate :: Expression -> Runtime Expression
evaluate (Foreign reference) = do
  reference' <- getForeign reference
  case reference' of
    Just (_, function) -> return function
    Nothing -> return $ Elementary (EError ("Reference error: foreign function does not exist"))
evaluate (Reference reference) = do
  reference' <- getEntity reference
  case reference' of
    Just (_, Lambda closureId parameters expression) -> do
    -- currently not supported to evaluate lambdas like this.
      return $ Lambda closureId parameters expression
    Just (_, expression) -> do
      result <- evaluate expression
      -- To avoid reevaluating expressions
      -- we update the referenced expression with its evaluated form.
      -- Potential optimization to handle the case where the referenced value
      -- is already an Elementary.
      addEntity reference (Nothing, result)
      return result
    Nothing -> return $ Elementary (EError ("Reference error: "++reference ++ " does not exist"))

evaluate (Lambda closureId parameters arguments) = return $ Lambda closureId parameters arguments
evaluate (Call expression args) = evaluateCall expression args
evaluate (Elementary expr) = return (Elementary expr)
evaluate (Tertiary condition left right) = do
  isTrue <- evaluate condition
  case isTrue of
    Elementary (EBool True) -> evaluate left
    Elementary (EBool False) -> evaluate right
    _ -> return $ Elementary (EError "Expected boolean in ternary condition")
-- comparisons
-- And
evaluate (Chain []) = return $ Chain []
evaluate (Chain xs) = Chain <$> (sequence $ (map evaluate xs))
evaluate (Binary And (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ Elementary (EBool (left && right))
evaluate (Binary Or (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ Elementary (EBool (left || right))
-- Equals
evaluate (Binary Equals (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EBool (left == right))
evaluate (Binary Equals (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EBool (left == right))
evaluate (Binary Equals (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EBool (left == right))
evaluate (Binary Equals (Elementary (EBool left)) (Elementary (EBool right))) =
  return $ Elementary (EBool (left == right))
-- NotEquals
evaluate (Binary NotEquals (Elementary left) (Elementary right)) =
  evalOpposite NotEquals Equals (Elementary left) (Elementary right)
-- Greater
evaluate (Binary Greater (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EBool (left > right))
evaluate (Binary Greater (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EBool (left > right))
evaluate (Binary Greater (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EBool (left /= right && (isSubstring right left)))
-- Less
evaluate (Binary Less (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EBool (left /= right && (isSubstring left right)))
evaluate (Binary Less (Elementary left) (Elementary right)) =
  evalOpposite Less Greater (Elementary left) (Elementary right)
-- Superset
evaluate (Binary SuperSet (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EBool (left >= right))
evaluate (Binary SuperSet (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EBool (left >= right))
evaluate (Binary SuperSet (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EBool (isSubstring right left))
-- Subset
evaluate (Binary SubSet (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EBool (left <= right))
evaluate (Binary SubSet (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EBool (left <= right))
evaluate (Binary SubSet (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EBool (isSubstring left right))
-- Multiply
evaluate (Binary Multiply (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EInt (left * right))
evaluate (Binary Multiply (Elementary (EFloat left)) (Elementary (EInt right))) =
  return $ Elementary (EFloat (left * (fromIntegral right)))
evaluate (Binary Multiply (Elementary (EInt left)) (Elementary (EFloat right))) =
  return $ Elementary (EFloat ((fromIntegral left) * right))
evaluate (Binary Multiply (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EFloat (left * right))
-- Divide
evaluate (Binary Divide (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $  Elementary (EFloat (left / right))
evaluate (Binary Divide (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EInt (div left right))
-- Minus
evaluate (Binary Minus (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EInt (left - right))
-- Plus
evaluate (Binary Plus (Elementary (EInt left)) (Elementary (EInt right))) =
  return $ Elementary (EInt (left + right))
evaluate (Binary Plus (Elementary (EFloat left)) (Elementary (EFloat right))) =
  return $ Elementary (EFloat (left + right))
evaluate (Binary Plus (Elementary (EString left)) (Elementary (EString right))) =
  return $ Elementary (EString (left ++ right))
evaluate (Binary Plus (Elementary (EError left)) (Elementary right)) =
  evalError Plus (EError left) right
evaluate (Binary Plus (Elementary left) (Elementary (EError right))) =
  evalError Plus (EError right) left
evaluate (Binary Plus (Elementary (EString left)) (Elementary right)) =
  return $ Elementary (EString ((left) ++ (showElement right)))
evaluate (Binary Plus (Elementary (left)) (Elementary (EString right))) =
  return $ Elementary (EString ((showElement left) ++ (right)))
evaluate (Binary op (Elementary left) (Elementary right)) =
  evalError op left right
evaluate (Binary op left right) = do
    left' <- evaluate left
    right' <- evaluate right
    evaluate (Binary
       op
       left'
       right')
evaluate expression = do
    return $ Elementary (EError $ "cant handle this case")
