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


interpret :: Bool -> [Statement] -> Runtime ()
interpret echo = mapM_ (interpreter echo)


assign :: [Modifier] -> String -> Expression -> Runtime ()
assign [Eager] name expression = do
    expression' <- evaluate expression
    addEntity name (Nothing, expression')

assign [] name expression = do
    addEntity name (Nothing, expression)

interpreter :: Bool -> Statement -> Runtime ()
interpreter echo stmt = case stmt of
  IfElse condition thenBlock elseBlock -> do
    result <- evaluate condition
    case result of
        (Elementary (EError e)) -> addError (e)
        (Elementary (EBool True)) -> interpreter False thenBlock
        (Elementary (EBool False)) -> interpreter False elseBlock
        _ -> addError ("Expected a boolean in if condition")
  Block thisBlock -> interpretBlock False thisBlock

  (Assign modifiers name expression) -> do
    assign modifiers name expression

  Print expr -> do
    liftIO $ print expr
  Weak expr -> do
    case expr of
      (Reference reference) -> do
        maybeEntity <- getEntity reference
        case maybeEntity of
          Just (meta, expression) -> do
            expression' <- evaluate expression
            addEntity reference (meta, expression')

            return ()
          Nothing -> addError ("Reference error")
      (Call expression arguments) -> do
            evaluate (Call expression arguments)
            return ()
      (Elementary (EError message)) -> addError (message)
      _ -> if echo
             then do
               expression' <- evaluate expr
               liftIO $ putStrLn (showHL expression')
             else
               return ()



interpretBlock :: Bool -> [Statement] -> Runtime ()
interpretBlock commit statements = do
  oldState <- get
  interpret False statements
  if commit
    then return ()
    else put oldState








