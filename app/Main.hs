module Main where
import Lib.VLex
import Lib.Data.Token
main :: IO ()
main = do
    putStrLn "Explore vektoria:"
    repl 0



repl :: Int -> IO ()
repl lnr = do
    line <- getLine
    let result = (run (vektoriaLex lnr)) line
    let tokens = fst (head result)
    print (map symbol tokens)
    let ast = (run vektoriaParse) tokens
    print ast
    let v = evalExpression $ head (fst (head ast))
    print v
    putStrLn ""
    repl (lnr+1)
