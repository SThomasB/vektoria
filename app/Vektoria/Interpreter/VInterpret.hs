module Vektoria.Interpreter.VInterpret
  ( interpret, interpreter, from, dereference)
  where
import Vektoria.Interpreter.Evaluator
import qualified Data.Text as T
import Vektoria.Lib.Data.Token
import Vektoria.Lib.Data.Statement
import Vektoria.Interpreter.Runtime
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM)
import Control.Monad.State
import System.CPUTime





-- what is a function?
-- Is it a reference to a block?
-- An Entity is then not a named expression
-- But a named expression | statement




interpret :: [Statement] -> Runtime ()
interpret = mapM_ interpreter


interpreter :: Statement -> Runtime ()
interpreter stmt = case stmt of
  IfElse condition thenBlock elseBlock -> do
    condition' <- dereference condition
    case condition' of
      ElemExpr (EError e) -> addError (EError e)
      _ -> do
        conditionResult <- evaluate condition'
        case conditionResult of
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
        result <- evaluate expr
        liftIO $ putStrLn (showElement result)
        addEntity ref (Entity ref (ElemExpr result))
  Print expr -> do
    result <- evaluate expr
    case result of
      (EError _) -> addError (result)
      (_) -> liftIO $ putStrLn (showElement result)
  Weak expr -> do
    expr'  <- dereference expr
    case expr' of
      (ElemExpr (EError e)) -> addError (EError e)
      e -> do
        result <- evaluate expr'
        liftIO $ print (showElement (result))





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




