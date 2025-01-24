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
  Nothing -> liftIO $ putStrLn ""
  (Just v) -> execute v


-- execute (Dispatch ((Assignment m n ps):stmts)) = work n
-- :: f (S a) = a
--    f N = N
--    [  (f, Dispatch [([expr], x), ])  ]
-- (f (S 1) ) <: Call         f                          [(Call S 1)]
--                            f                          [ Call (Member "type" "S" 1) [1] ]
--                                                       [ Instance "S" expr ]
--               Call (Dispatch [([Call r"S" [ a ] ], xpr)])
--                                .-------^     .--------------------^
--                          when calle    == MemberName
--                          then bind args [ a ] to [ 1 ] in evaluate xpr
--
-- :: Option = {Some x, Nothing}
-- :: optAdd (Some x) (Some y) = x + y

execute (Weak (Elementary (EError message))) = addError (message)

execute (Weak (Reference reference)) = do
  maybeEntity <- getEntity reference
  case maybeEntity of
    Just (meta, expression) -> do
      expression' <- evaluate expression
      addEntity reference (meta, expression')

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
  addEntity name (Nothing, expression)

execute (Assign [Eager] name expression) = do
    expression' <- evaluate expression
    addEntity name (Nothing, expression')

execute (Reflect (Elementary (EString "state"))) = do
  state <- get
  liftIO $ print state

execute (Reflect (Elementary (EString s))) = liftIO $ putStrLn (s ++ "is not a known reflection")








