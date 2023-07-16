module Main where
import Lib.VLex
import Lib.VParse
import Lib.VInterpret
import Lib.Data.Token
import System.Environment
import System.IO
import Control.Monad (foldM)
main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> do
        putStrLn "Explore vektoria:"
        repl initState 0
      [arg] -> do
        content <- readFile (head args)
        let tokens = zipWith ($) (map (run . vektoriaLex) [1..]) (lines content)
        error <- mapM check tokens
        let ast = map (run vektoriaParse) (map getToken tokens)
        mapM_ (\t->do putStrLn "" ; putStrLn $ show t) ast
        interpret initState (map getStatement ast)
        print "-vektoria-"
      _ -> putStrLn "what?"

getToken :: [([Token], String)] -> [Token]
getToken [] = []
getToken [(t, _)] = t

getStatement :: [([Statement], [Token])] -> [Statement]
getStatement [] = []
getStatement [(statement, _)] = statement


interpret :: VState -> [[Statement]] ->IO VState
interpret state [] = return state
interpret state statements = do
    finalState <- foldM interpreter state statements
    return  state

interpreter :: VState -> [Statement] -> IO VState
interpreter state [] = return state
interpreter state [stmt] = case stmt of
  Assign expr -> do
    return $ interpretAssign state (Assign expr)
  Print expr -> do
    print (evalExpr (dereference state expr))
    return state
  Weak expr -> do
    let v = evalExpr (dereference state expr)
    print v
    return state

check :: [([Token], String)] -> IO Bool
check [] = do
    return True

check [(t, [])] = do
  return True

check [(t, s)] = do
  putStrLn $ "Unexpected token on line "++(show $ line (head t))++": " ++ (concat (map lexeme t)) ++ s
  putStrLn $ "Could not parse: "++(show s)
  return False

repl :: VState -> Int -> IO ()
repl state lnr = do
    line <- getLine
    let result = (run (vektoriaLex lnr)) line
    let tokens = fst (head result)
    print (map symbol tokens)
    let ast = (run (vektoriaParse)) tokens
    let stmt = head (fst (head ast))
    case stmt of
      Assign e -> repl (interpretAssign state (Assign e)) (lnr+1)
      Print e -> print (evalExpr (dereference state e))
      Weak e -> do
        let expr = dereference state e
        let derefExpr = (dereference state expr)
        print $ (show e) ++ "->" ++(show derefExpr)
        print expr
        print $ evalExpr (derefExpr)
    repl state (lnr + 1)


