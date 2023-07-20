module Vektoria.Interpreter.VInterpret
  ( interpret, interpreter )
  where
import qualified Data.Text as T
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Entity
import Vektoria.Lib.Data.Element
import Vektoria.Lib.Data.Expression
import Vektoria.Interpreter.Evaluator
import Vektoria.Interpreter.Runtime
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM)
import Control.Monad.State


interpret :: [Statement] -> Runtime ()
interpret = mapM_ interpreter


interpreter :: Statement -> Runtime ()
interpreter stmt = case stmt of
  IfElse condition thenBlock elseBlock -> do
    result <- evaluate condition
    case result of
        (EError e) -> addError (e)
        EBool True -> interpreter thenBlock
        EBool False -> interpreter elseBlock
        _ -> addError ("Expected a boolean in if condition")
  Block thisBlock -> interpretBlock False thisBlock
  (Assign name (Lambda parameters expression)) -> do
    addEntity name (Callable parameters expression)
  (Assign name (Ref reference)) -> do
    maybeEntity <- getEntity reference
    case maybeEntity of
      Just entity -> addEntity name entity
      Nothing -> addError ("Reference error")
  (Assign name expression) -> do
    expression' <- dereference expression
    addEntity name (Computable expression')
  Print (Ref ref) -> do
    result <- evaluate (Ref ref)
    case result of
      (EError message) -> do
        addError (message)
        liftIO $ putStrLn "An error occurred"
      _ -> do
        liftIO $ putStrLn (showElement result)
  Print expr -> do
    result <- evaluate expr
    case result of
      (EError message) -> do
        addError (message)
        liftIO $ putStrLn ("An error occurred in: "++(show stmt))
      (_) -> liftIO $ putStrLn (showElement result)
  Weak expr -> do
    expr'  <- dereference expr
    case expr' of
      (ElemExpr (EError message)) -> addError (message)
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








