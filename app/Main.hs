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
  isValid <- allM check lexedLines
  if isValid
    then do
      let tokenStream = concat ((map getTokens) lexedLines)
      let ast = runParse tokenStream
      let statementStream = concat (map fst ast)
      if 10==10
        then do
          _ <- interpret initState statementStream
          putStrLn ""
        else putStrLn "Syntax error"
      putStrLn ""
    else putStrLn "Unexpected token"

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



