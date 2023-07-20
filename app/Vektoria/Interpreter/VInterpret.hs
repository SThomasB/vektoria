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
  (Assign name expr) -> do
    expr' <- dereference expr
    addEntity name (Computable expr')
  Print (Ref ref) -> do
    ref' <- dereference (Ref ref)
    case ref' of
      (ElemExpr (EError message)) -> addError (message)
      expr -> do
        result <- evaluate expr
        liftIO $ putStrLn (showElement result)
        addEntity ref (Computable (ElemExpr result))

  Print expr -> do
    result <- evaluate expr
    case result of
      (EError message) -> addError (message)
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








