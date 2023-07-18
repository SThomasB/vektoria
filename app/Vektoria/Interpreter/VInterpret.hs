module Vektoria.Interpreter.VInterpret
  ( interpret, interpreter, EntityMap, initState, from, dereference)
  where
import Vektoria.Interpreter.Evaluator
import qualified Data.Text as T
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Statement
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM)
import Control.Monad.State



type EntityMap = HashMap.HashMap String Entity

named :: EntityMap -> String -> (Maybe Entity)
entities `named` name = HashMap.lookup name entities

data RuntimeState = RuntimeState
  { entities :: EntityMap
  , errors :: [Element]
  } deriving (Show)



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
  Block thisBlock -> interpret thisBlock
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
  Weak expr -> do
    expr'  <- dereference expr
    case expr' of
      (ElemExpr (EError e)) -> addError (EError e)
      e -> liftIO $ print (evaluate expr)


dereference :: Expression -> Runtime Expression
dereference (Ref r) = do
  r' <- getEntity r
  case r' of
    Just (Entity name thing) -> return thing
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


initState :: EntityMap
initState = HashMap.empty

--interpretAssign :: EntityMap -> Statement -> EntityMap
--interpretAssign state (Assign e) = HashMap.insert (name e) Entity {name=(name e), thing=(dereference state (thing e))} state




