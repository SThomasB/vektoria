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

    let v = case ast of
            [(parsed, [])] -> (evalExpression $ head parsed)
            [(parsed, notParsed)] -> EError ("Syntax Error, could not parse: "++ (show notParsed))
            _ -> EError ("Error")
    print v
    putStrLn ""
    repl (lnr+1)
