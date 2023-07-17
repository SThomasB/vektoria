module Main where
import Vektoria.Lexer.VLex
import Vektoria.Parser.VParse
import Vektoria.Interpreter.VInterpret
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Token
import System.Environment
import System.IO
import Control.Monad (foldM)
main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> repl initState 0
      [filePath] -> do interpretFile filePath
      _ -> putStrLn "Invalid arguments."



interpretFile :: String -> IO ()
interpretFile filePath = do
  content <- readFile filePath
  let lexedLines = zipWith runLex [1..] (lines content)
  isValid <- allM check lexedLines
  if isValid
    then do
      let ast = map (runParse . getTokens) lexedLines
      _ <- interpret initState (map getStatements ast)
      putStrLn ""
    else putStrLn "Syntax error."


allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = foldM (\acc x -> if acc then p x else return False) True

runLex :: Int -> String -> [([Token], String)]
runLex lineNr line = (run . vektoriaLex) lineNr line

runParse :: [Token] -> [([Statement], [Token])]
runParse = run vektoriaParse

printAst :: [([Statement], [Token])] -> IO ()
printAst ast = putStrLn "" >> mapM_ (putStrLn . show) ast

getTokens, getStatements :: [([a], b)] -> [a]
getTokens = concatMap fst
getStatements = concatMap fst


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


