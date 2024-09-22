module Main where
import Vektoria.Lexer.VLex
import Vektoria.Parser.VParse
import Vektoria.Interpreter.VInterpret
import Vektoria.Lib.Data.Statement
import Vektoria.Lib.Data.Token
import Vektoria.Interpreter.Runtime
import System.Environment
import System.IO
import Control.Monad (foldM, forM, mapM_, mapM)
import Control.Monad.State (runStateT)




userStream = lines <$> getContents
lexify = zipWith runLex [1..]
buildAST = map (getStatements . runParse . getTokens)



runInterpreter statements = runStateT (interpret True statements) initRuntime



main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> ((map head) <$> buildAST <$> lexify <$> userStream) >>= runInterpreter >> putStrLn ""
      ["--lex"] -> (zipWith runLex [1..] ) <$> (lines <$> getContents) >>= (mapM_ print)
      ["--ast"] -> buildAST <$> lexify <$> userStream >>= (mapM_ print)
      ["--de", prg] -> ((map head) <$> buildAST <$> (pure $ lexify (lines prg))) >>=(\s -> runStateT (interpret True s) initRuntime) >> putStrLn ""
      [filePath] -> do interpretFile filePath
      _ -> putStrLn "Invalid arguments."



interpretFile :: String -> IO ()
interpretFile filePath = do
  content <- readFile filePath
  --print content
  let lexedLines = zipWith runLex [1..] (lines content)
  --print lexedLines
  isValid <- allM check lexedLines
  if isValid
    then do
      let tokenStream = concat (filter (notComment) $ (map getTokens) lexedLines)
      let ast = runParse tokenStream
      --print ast
      isValidAst <- checkAst ast
      if isValidAst
        then do
          let statementStream = concat (map fst ast)
          (_, finalState) <- runStateT (interpret False statementStream) initRuntime
          --putStrLn ""
          --putStrLn "Accrued errors:"
          --mapM print (zip [1..] (errors finalState))
          --print (finalState)
          return ()
        else return ()
    else do
      putStrLn "Invalid tokens"
      return ()

notComment :: [Token] -> Bool
notComment [] = True
notComment (t:tokens) = (symbol t) /= SMinusMinus

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
  putStrLn $ "Lexical error: "++(show s)
  return False

checkAst :: [([Statement], [Token])] -> IO Bool
checkAst [] = do
    return True

checkAst [(t, [])] = do
  return True

checkAst [(t, s)] = do
  putStrLn $ "Syntax error: "++(show $ head s)
  return False



