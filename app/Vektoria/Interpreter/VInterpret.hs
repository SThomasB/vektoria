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


interpret :: Bool -> [Maybe Statement] -> Runtime ()
interpret echo = mapM_ (interpreter echo)


assign :: [Modifier] -> String -> Expression -> Runtime ()
assign [Eager] name expression = do
    expression' <- evaluate expression
    addEntity name (Nothing, expression')

assign [] name expression = do
    addEntity name (Nothing, expression)

interpreter :: Bool -> Maybe Statement -> Runtime ()
interpreter echo stmt = case stmt of
  Nothing -> liftIO $ putStrLn "!syntax error"
  (Just v) -> case v of
    (Reflect (Elementary (EString s))) -> do
      case s of
        "state" -> do
          state <- get
          liftIO $ print state
        _ -> liftIO $ putStrLn (s ++ "is not a known reflection")
    (Assign modifiers name expression) -> do
      assign modifiers name expression
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









