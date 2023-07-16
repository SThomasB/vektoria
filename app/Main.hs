module Main where
import Lib.VLex
import Lib.VParse
import Lib.VInterpret
import Lib.Data.Token

main :: IO ()
main = do
    putStrLn "Explore vektoria:"
    repl initState 0



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
      Print _ -> print state
      _ -> print stmt
    repl state (lnr + 1)


