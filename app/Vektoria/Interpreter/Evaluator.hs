{-# LANGUAGE TupleSections #-}

module Vektoria.Interpreter.Evaluator (evaluate) where
import Vektoria.Lib.Data.Expression
import Vektoria.Interpreter.Runtime
import Control.Monad
import Data.Unique
import Data.List (repeat)
import qualified Data.Text as T

resolve :: [String] -> Expression -> Runtime Expression

resolve protected (Reference reference) = do
  if reference `elem` protected
    then return $ Reference reference
    else entity reference

resolve  protected (Call callee arguments) = do
  callee' <- resolve protected callee
  arguments' <- mapM (resolve protected) arguments
  return $ Call callee' arguments'

resolve  protected (Ternary condition left right) = do
  condition' <- resolve protected condition
  left' <- resolve protected left
  right' <- resolve protected right
  return $ Ternary condition' left' right'


resolve  protected (Binary op left right) = do
  left' <- resolve protected left
  right' <- resolve protected right
  return $ Binary op left' right'

resolve protected (Elementary (Lambda n closure parameters expression)) = do
  expression' <- resolve parameters expression
  ret $ Lambda n closure parameters expression

resolve  _ (expression) = return expression


ret :: Element -> Runtime Expression
ret value = return $ Elementary value


toElement :: Expression -> Element
toElement (Elementary element) = element
toElement expression = EProxy expression
evaluate :: Expression -> Runtime Expression
-- Elementary: Vektoria expressions a
--  simply returns the expression as is
evaluate elementary@(Elementary _) = return elementary

-- Binary: Vektoria expressions a `op` b
--  evaluates a then 
--  evaluates b then 
--  evaluates the operator
evaluate binaryExpr@(Binary op x y)   = do 
  a' <- toElement <$> evaluate x
  b' <- toElement <$> evaluate y
  case (op, a', b') of
    -- And: Vektoria expressions a && b
    (And, EBool a, EBool b)        -> ret $ EBool (a && b)
    (And,_ ,_)                     -> ret $ EError 
                                          $ "Type mismatch in a && b; " <> (show binaryExpr)
    
    -- Or: Vektoria expressions a || b
    (Or, EBool a, EBool b)         -> ret $ EBool (a || b)
    (Or, _, _)                     -> ret $ EError 
                                          $ "Type mismatch in a || b; " <> (show binaryExpr)

    -- Equals: Vektoria expression a == b
    (Equals, EBool a, EBool b)     -> ret $ EBool (a == b)
    (Equals, EString a, EString b) -> ret $ EBool (a == b)
    (Equals, EInt a, EInt b)       -> ret $ EBool (a == b)
    (Equals, EFloat a, EFloat b)   -> ret $ EBool (a == b)
     
    -- Plus: Vektoria expressions a + b
    (Plus, EInt a, EInt b)         -> ret $ EInt   (a + b)
    (Plus, EFloat a, EFloat b)     -> ret $ EFloat (a + b)
    (Plus, _, _)                   -> ret $ EError 
                                          $ "Type mismatch in a + b; " <> (show binaryExpr)

    -- Minus: Vektoria expressions a - b
    (Minus, EInt a, EInt b)         -> ret $ EInt   (a - b)
    (Minus, EFloat a, EFloat b)     -> ret $ EFloat (a - b)
    (Minus, _, _)                   -> ret $ EError 
                                           $ "Type mismatch in a - b; " <> (show binaryExpr)
    -- Multiply: Vektoria expressions a * b
    (Multiply, EInt a, EInt b)         -> ret $ EInt    (a * b)
    (Multiply, EFloat a, EFloat b)     -> ret $ EFloat  (a * b)
    (Multiply, _, _)                   -> ret $ EError 
                                           $ "Type mismatch in a * b; " <> (show binaryExpr)
    
    -- Divide: Vektoria expressions a * b
    (Divide, EInt a, EInt b)      | b == 0    -> ret $ EError 
                                                 $ "Divide by 0 a / b; " <> (show binaryExpr)
                                  | otherwise -> ret $ EInt    (a `div` b)
    (Divide, EFloat a, EFloat b)  | b == 0    -> ret $ EError
                                                     $ "Divide by 0 a / b; " <> (show binaryExpr)
                                  | otherwise -> ret $ EFloat  (a / b)
    (Divide, _, _)  -> ret $ EError 
                           $ "Type mismatch in a * b; " <> (show binaryExpr)

    -- Greater: Vektoria expressions a > b
    (Greater, EInt a, EInt b)     -> ret $ EBool (a > b)
    (Greater, EFloat a, EFloat b) -> ret $ EBool (a > b)
    (Greater, _, _)               -> ret $ EError
                                         $ "Type mismatch in a > b; " 
                                           <> (show binaryExpr)

    -- Less: Vektoria expressions a < b
    (Less, EInt a, EInt b)     -> ret $ EBool (a < b)
    (Less, EFloat a, EFloat b) -> ret $ EBool (a < b)
    (Less, _, _)               -> ret $ EError
                                      $ "Type mismatch in a < b; " 
                                        <> (show binaryExpr)

    -- SuperSet: Vektoria expressions a >= b
    (SuperSet, EInt a, EInt b)     -> ret $ EBool (a >= b)
    (SuperSet, EFloat a, EFloat b) -> ret $ EBool (a >= b)
    (SuperSet, _, _)               -> ret $ EError
                                          $ "Type mismatch in a >= b; " 
                                             <> (show binaryExpr)
    -- SubSet: Vektoria expressions a <= b
    (SubSet, EInt a, EInt b)     -> ret $ EBool (a <= b)
    (SubSet, EFloat a, EFloat b) -> ret $ EBool (a <= b)
    (SubSet, _, _)               -> ret $ EError
                                        $ "Type mismatch in a <= b; " 
                                           <> (show binaryExpr)
    (Tilde, Lambda name _ _ _, Lambda name' _ _ _) -> ret $ EBool (name == name')
    _ -> ret $ EError $ "Operator not supported " <> show a' <> " " <> (show op) <> " " <> show b'

-- Call: Vektoria expressions (x a b c ..)
evaluate call@(Call x' arguments) = do
  x <- evaluate x'
  case x of
    -- Callee evaluates to a lambda
    (Elementary (Lambda n closure parameters expression)) 
      -- Check if the number of parameters expected
      -- by the lambda expression is the same as 
      -- the number of supplied arguments.
      | arity == numSuppliedArgs -> do
        evaluatedArgs <- mapM evaluate arguments
        let newBindings = (parameters `zip` evaluatedArgs) :: [(String, Expression)]
        let bindings = (newBindings ++ closure)
        preCallScope <- get
        put $ newScope preCallScope bindings
        result <- evaluate expression
        result' <- do
          case result of
            (Elementary (Lambda nn closure parameters expression)) -> do
              expression' <- resolve parameters expression
              ret $ Lambda (if nn=="" then n else nn) closure parameters expression'

            _ -> return result
        put preCallScope
        return result'
      | otherwise ->
        -- Partial lambda application case
        let (paramsToBindNow, paramsToBindLater) = splitAt numSuppliedArgs parameters
            boundParams = paramsToBindNow `zip` arguments
        in ret $ Lambda n boundParams paramsToBindLater expression
      where arity           = length parameters
            numSuppliedArgs = length arguments
            name            = case x' of
              (Reference r) -> r
              _ -> ""

    -- IOAction: Vektoria expressions @name x y z..
    -- where name is linked to an IO action in the Runtime
    (IOAction action) -> 
           mapM evaluate arguments
           >>= liftIO . action

    -- Primitive: Vektoria expressions @name x y z..
    -- where name is linked to a pure function in the Runtime
    (Primitive function) ->
           mapM evaluate arguments
           >>= return . function

    -- Error case
    _ -> ret $ EError 
             $ "Illegal call in (x a b ..); " <> (show x) 
  
-- Reference: Vektoria expressions @a
-- where a is registered in the Runtime as either
-- IOAction or Primitive
evaluate ref@(Foreign reference) = do
  reference' <- getForeign reference
  case reference' of
    Just function -> return function
    Nothing -> ret $ EError $ "Reference error; " <> (show ref)

-- Reference: Vektoria expressions a
-- where :: a = b has been encountered
evaluate (Reference reference) = do
  entity <- getEntity reference
  case entity of
    Just expression -> do
      result <- evaluate expression
      return result
    Nothing -> ret $ EError $ "Not found: " <> reference


evaluate ternary@(Ternary condition' left right) = do
  condition <- evaluate condition'
  case condition of
    Elementary (EBool True)  -> evaluate left
    Elementary (EBool False) -> evaluate right
    _ -> ret $ EError $ "Expected boolean in ? a -> b | c; " <> show ternary

evaluate expression = 
    ret $ EError $ "No evaluation; " <> show expression
