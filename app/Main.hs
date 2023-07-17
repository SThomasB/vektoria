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
      [] -> interpretFile "test.vk"
      [filePath] -> do interpretFile filePath
      _ -> putStrLn "Invalid arguments."


interpretFile :: String -> IO ()
interpretFile filePath = do
  content <- readFile filePath
  let lexedLines = zipWith runLex [1..] (lines content)
  mapM_ print lexedLines
  isValid <- allM check lexedLines
  if isValid
    then do
      let tokenStream = concat ((map getTokens) lexedLines)
      print tokenStream
      let ast = runParse tokenStream
      let statementStream = concat (map fst ast)
      if 10==10
        then do
          _ <- interpret initState statementStream
          putStrLn ""
        else putStrLn "Syntax error"
      putStrLn ""
    else putStrLn "Unexpected token"

showElement :: Element -> String
showElement (EString v) = v
showElement (EInt v) = show v
showElement (EFloat v) = show v
showElement (EError v) = v
showElement _ = ""
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


interpret :: VState -> [Statement] ->IO VState
interpret state [] = return state
interpret state statements = do
    finalState <- foldM interpreter state statements
    return state

interpreter :: VState -> Statement -> IO VState
interpreter state stmt = case stmt of
  IfElse condition thenBlock elseBlock -> do
    case evalExpr (dereference state condition) of
      EBool True -> interpreter state thenBlock
      EBool False -> interpreter state elseBlock
      _ -> return state
  Block thisBlock -> do
    interpret state thisBlock
    return state
  Assign expr -> do
    return $ interpretAssign state (Assign expr)
  Print expr -> do
    putStrLn $ showElement (evalExpr (dereference state expr))
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

checkAst :: [([Statement], [Token])] -> IO Bool
checkAst [] = do
    return True

checkAst [(t, [])] = do
  return True

checkAst [(t, s)] = do
  putStrLn $ "Could not parse: "++(show s)
  return False



