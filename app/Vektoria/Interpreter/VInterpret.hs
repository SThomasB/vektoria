module Vektoria.Interpreter.VInterpret
  ( interpret, interpreter, errors, EntityMap, RuntimeState, initRuntime, from, dereference)
  where
import Vektoria.Interpreter.Evaluator
import qualified Data.Text as T
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Statement
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM)
import Control.Monad.State
import System.CPUTime


type EntityMap = HashMap.HashMap String Entity

named :: EntityMap -> String -> (Maybe Entity)
entities `named` name = HashMap.lookup name entities







initEntityMap :: EntityMap
initEntityMap = HashMap.fromList [
  ("add", Callable ["a", "b", "c"] (Binary Plus (Ref "a") (Binary Plus (Ref "b") (Ref "c"))))]


getSysTime :: [Element] -> IO Expression
getSysTime [] = do
  cpuTimeNano <- getCPUTime
  return $ ElemExpr (EFloat $ fromIntegral cpuTimeNano)
getSystem [s] = return $ ElemExpr (EError "getSysTime takes zero arguments")



-- what is a function?
-- Is it a reference to a block?
-- An Entity is then not a named expression
-- But a named expression | statement


data RuntimeState = RuntimeState
  { entities :: EntityMap
  , errors :: [Element]
  } deriving (Show)


initRuntime::RuntimeState
initRuntime = RuntimeState {entities=initEntityMap, errors=[]}

type Runtime a = StateT RuntimeState IO a

getEntity :: String -> Runtime (Maybe Entity)
getEntity name = do
    entities <- gets entities
    return $ entities `named` name


addEntity :: String -> Entity -> Runtime ()
addEntity name value = modify $ \s -> s { entities = HashMap.insert name value (entities s) }

addError :: Element -> Runtime ()
addError err = modify $ \s -> s { errors = errors s ++ [err] }


interpret :: [Statement] -> Runtime ()
interpret = mapM_ interpreter


interpreter :: Statement -> Runtime ()
interpreter stmt = case stmt of
  IfElse condition thenBlock elseBlock -> do
    condition' <- dereference condition
    case condition' of
      ElemExpr (EError e) -> addError (EError e)
      _ -> case evaluate condition' of
        EBool True -> interpreter thenBlock
        EBool False -> interpreter elseBlock
        _ -> addError (EError "Expected a boolean in if condition")
  Block thisBlock -> interpretBlock False thisBlock
  Assign (Entity name expr) -> do
    expr' <- dereference expr
    addEntity name (Entity name expr')
  Print (Ref ref) -> do
    ref' <- dereference (Ref ref)
    case ref' of
      (ElemExpr (EError e)) -> addError (EError e)
      expr -> do
        let result = evaluate expr
        liftIO $ putStrLn (showElement result)
        addEntity ref (Entity ref (ElemExpr result))
  Print expr -> do
    expr' <- dereference expr
    case expr' of
      (ElemExpr (EError e)) -> addError (EError e)
      _ -> do
        result <- interpretEvaluation expr'
        case result of
          Nothing -> return ()
          Just result' -> liftIO $ putStrLn (showElement result')
  Weak expr -> do
    expr'  <- dereference expr
    case expr' of
      (ElemExpr (EError e)) -> addError (EError e)
      e -> liftIO $ print (showElement (evaluate expr'))


makeScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
makeScope state newEntities =
    state { entities = HashMap.union (HashMap.fromList newEntities) (entities state)}


evaluateArguments :: [String]->[Expression] -> Runtime (Maybe [(String, Entity)])
evaluateArguments bindings args = do
  let boundArgs = zip bindings args
  maybeResults <- mapM evaluateArg boundArgs
  return (sequence maybeResults)
  where
    evaluateArg (binding, arg) = do
      expr <- dereference arg
      elem <- interpretEvaluation expr
      case elem of
        Nothing -> do
          addError (EError "Error evaluating arguments")
          return Nothing
        Just elem -> return $ Just (binding, Entity binding (ElemExpr elem))


interpretEvaluation :: Expression -> Runtime (Maybe Element)
interpretEvaluation (Call (Ref ref) args) = do
    ref' <- getEntity ref
    case (ref') of
      (Just (Callable bindings expr)) -> do
        let arity = (length bindings)
        let nrArgs = (length args)
        if (arity == nrArgs)
          then do
            args' <- evaluateArguments bindings args
            case args' of
              Nothing -> return Nothing
              Just validBindings -> do
                oldState <- get
                let functionScope = makeScope oldState validBindings
                put functionScope
                executableFunction <- dereference (expr)
                maybeElem <- interpretEvaluation executableFunction
                put oldState
                return maybeElem
          else return Nothing
      _ -> return Nothing
interpretEvaluation expr = do
    let result = evaluate expr
    case result of
      (EError e) -> do
        addError (EError $ e ++" in: "++(show expr))
        return Nothing
      _ -> return $ Just result

interpretBlock :: Bool -> [Statement] -> Runtime ()
interpretBlock commit statements = do
  oldState <- get
  interpret statements
  if commit
    then return ()
    else put oldState

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


from :: String -> EntityMap -> Expression
a `from` s = case HashMap.lookup a s of
  Just e -> thing e
  Nothing -> ElemExpr (EError (a ++ " does not exist"))



--interpretAssign :: EntityMap -> Statement -> EntityMap
--interpretAssign state (Assign e) = HashMap.insert (name e) Entity {name=(name e), thing=(dereference state (thing e))} state




