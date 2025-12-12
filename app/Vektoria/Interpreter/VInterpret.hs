module Vektoria.Interpreter.VInterpret
  ( interpret, interpreter )
  where
import qualified Data.Text as T
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Expression
import Vektoria.Interpreter.Evaluator
import Vektoria.Interpreter.Runtime
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (foldM)
import Control.Monad.State


interpret :: Bool -> [Maybe Statement] -> Runtime ()
interpret echo = mapM_ (interpreter echo)


assign :: [Modifier] -> String -> Expression -> Runtime ()
--assign _ name (Reference ref) = do
--  dereffed <- entity ref
--  case dereffed 
assign [Eager] name expression = do
    expression' <- evaluate expression
    addEntity name expression'

assign [] name expression = do
    addEntity name expression

interpreter :: Bool -> Maybe Statement -> Runtime ()
interpreter echo stmt = case stmt of
  Nothing -> liftIO $ putStrLn ""
  (Just v) -> execute v



execute (Weak (Elementary (EError message))) = addError (message)

execute (Weak (Reference reference)) = do
  maybeEntity <- getEntity reference
  case maybeEntity of
    Just expression -> do
      expression' <- evaluate expression
      addEntity reference expression'
      return ()
    Nothing -> addError ("Reference error")

execute (Weak (Call expression arguments)) = do
  evaluate (Call expression arguments)
  return ()

execute (Weak expr) = do
  echo <- getEcho
  expression' <- evaluate expr
  if echo
  then liftIO $ putStrLn (showHL expression')
  else return ()

execute  (Assign [] name expression) = do
  case expression of 
    (Elementary (Lambda name' state params body)) -> 
      addEntity name $ Elementary (Lambda (if name' == "" then name else name') state params body)
    _ ->  addEntity name expression

execute (Assign [Eager] name expression) = do
    expression' <- evaluate expression
    execute (Assign [] name expression')

execute (Reflect (Elementary (EString "state"))) = do
  state <- get
  liftIO $ print state

execute (Reflect (Elementary (EString s))) = liftIO $ putStrLn (s ++ "is not a known reflection")








