module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Vektoria.Lib.Data.Expression
import Vektoria.Lib.Data.Element
import Data.Time.Clock.System
import Data.Unique
import Data.Char (ord)
import Data.List.Split
import Data.List (intercalate, isPrefixOf)
import System.Random (randomRIO)
import System.Directory
import System.Environment
import System.IO (withFile, IOMode(ReadMode), hGetContents, hFlush, stdout)
import Control.Concurrent (threadDelay)
type Runtime a = StateT RuntimeState IO a
type Scope = HashMap.HashMap String Entity
type Entity = (Maybe Metadata, Expression)

data Metadata = Metadata {
    typeSignature :: Maybe String
} deriving Show

emptyMetadata :: Metadata
emptyMetadata = Metadata {typeSignature=Nothing}

data RuntimeError = RuntimeError {
    message :: String
} deriving (Show)
data RuntimeEnv = RuntimeEnv {
  echo :: Bool
}
data RuntimeState = RuntimeState
  { scope :: Scope
  , ffi :: Scope
  , errors :: [RuntimeError]
  , env :: RuntimeEnv
  }
instance Show RuntimeState where
  show s = (show (scope s ))

initRuntime :: RuntimeEnv -> RuntimeState
initRuntime rte = RuntimeState {scope=initScope, ffi=initFFI, errors=[], env=rte}

getEntity, getForeign :: String -> Runtime (Maybe Entity)
getEntity name = do
    scope <- gets scope
    return $ scope `named` name

getForeign name = do
  foreignEntities <- gets ffi
  return $ foreignEntities `named` name

getEcho :: Runtime Bool
getEcho = do
  rte <- gets env
  return $ echo rte

addEntity :: String -> (Maybe Metadata, Expression) -> Runtime ()
addEntity name (metadata, expression) = modify $ \s -> s { scope = HashMap.insert name (metadata, expression) (scope s) }

addError :: String -> Runtime ()
addError message = modify $ \s -> s { errors = (errors s) ++ [RuntimeError message] }

initScope, initFFI :: Scope
initScope = HashMap.fromList [("Write", (Nothing, Member "FileMode" "Write" ["n"]))]
initFFI = HashMap.fromList foreignFunctions

named :: Scope -> String -> (Maybe Entity)
scope `named` name = HashMap.lookup name scope

newScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
newScope state newEntities =
    state { scope = HashMap.union (HashMap.fromList newEntities) (scope state)}


foreignFunctions :: [(String, Entity)]
foreignFunctions =
                 [("print", (Nothing, IOAction printFFI))
                 ,("probe", (Nothing, IOAction probeFFI))
                 ,("user" , (Nothing, IOAction getInputFFI))
                 ,("userInt", (Nothing, IOAction getIntFFI))
                 ,("cpuTime", (Nothing, IOAction cpuTimeFFI))
                 ,("file", (Nothing, IOAction fileFFI))
                 ,("isFile", (Nothing, IOAction isFileFFI))
                 ,("randInt", (Nothing, IOAction randIntFFI))
                 ,("folder", (Nothing, IOAction folderFFI))
                 ,("args", (Nothing, IOAction argsFFI))
                 ,("len", (Nothing, Primitive lenFFI))
                 ,("ascii", (Nothing, Primitive asciiFFI))
                 ,("split", (Nothing, Primitive splitFFI))
                 ,("indexed", (Nothing, Primitive indexedFFI))
                 ,("count", (Nothing, Primitive countFFI))
                 ,("write", (Nothing, IOAction writeFFI))
                 ,("[]", (Nothing, Primitive listFFI))
                 ,("directory", (Nothing, IOAction directoryFFI))
                 ,("producer", (Nothing, Primitive producerFFI))
                 ,("toString", (Nothing, Primitive toStringFFI))
                 ,("clearScreen", (Nothing, IOAction clearScreenFFI))
                 ,("sleep", (Nothing, IOAction sleepFFI))
                 ,("escape", (Nothing, IOAction escapeFFI))
                 ]
interpretEscapes :: String -> String
interpretEscapes "" = ""
interpretEscapes ('\\':rest) =
  case rest of
    -- Handle common escape sequences
    'n':xs   -> '\n' : interpretEscapes xs
    't':xs   -> '\t' : interpretEscapes xs
    '\\':xs  -> '\\' : interpretEscapes xs
    '"':xs   -> '\"' : interpretEscapes xs
    'E':'S':'C':xs -> '\x1b' : interpretEscapes xs -- Match \ESC
    _        -> '\\' : interpretEscapes rest -- Treat unknown escapes literally
interpretEscapes (x:xs) = x : interpretEscapes xs
escapeFFI [Elementary (EString s)] = do
  let s' = interpretEscapes s
  return $ Elementary (EString s')


sleepFFI [(Elementary (EInt x))]= do
  threadDelay (x)
  return $ Elementary EVoid
sleepFFI _= return $ Elementary (EError "sleep is missing an argument")
toStringFFI [x] = Elementary (EString $ showHL x)
producerFFI (f:[p]) = Producer f (Chain []) p
listFFI :: [Expression] -> Expression
listFFI xs = Chain xs
countFFI :: [Expression] -> Expression
countFFI ((Elementary (EInt start)):[(Elementary (EInt stop))]) = Chain $ map intoElementaryInt [start..stop]
indexedFFI :: [Expression] -> Expression
indexedFFI ([UnChain expr]) = indexedFFI expr
indexedFFI exprs = Chain (map indexedToExpr $ zip [0..] exprs)
  where indexedToExpr (i, expr) = (Chain [(Elementary (EInt i)), expr])

isFileFFI [(Elementary (EString name))] = do
  exists <- doesFileExist name
  return $ Elementary (EBool exists)
splitFFI :: [Expression] -> Expression
splitFFI [Elementary (EString pattern), Elementary (EString v)] = (Chain $ map intoElementaryString (splitOn pattern v))
splitFFI _ = (Elementary (EError "illegal arguments"))
asciiFFI :: [Expression] -> Expression
asciiFFI [Elementary (EString (v:[]))] = Elementary
  $ EInt
  $ ord
  $ v
asciiFFI _ = Elementary
  $ EError
  $ "ascii is only defined for single characters"
lenFFI :: [Expression] -> Expression
lenFFI [Chain (expressions)] = Elementary
  $ EInt
  $ length
  $ expressions
lenFFI [(Elementary (EString v))] = Elementary
 $ EInt
 $ length
 $ v

lenFFI _ = Elementary $ (EError "len is only defined for chains")

argsFFI [] = do
  args <- getArgs
  return $ Chain (map intoElementaryString args)

argsFFI _ = return $ Elementary $ (EError "illegal argument for @args")

folderFFI :: [Expression] -> IO Expression
folderFFI [] = do
    paths <- getDirectoryContents "."
    return $ Chain (map intoElementaryString paths)
folderFFI [Elementary (EString path)] = do
    paths <- getDirectoryContents path
    return $ Chain (map intoElementaryString paths)
folderFFI _ = return $ Elementary $ (EError "illegal argument for @folder")
directoryFFI [] = do
  path <- getCurrentDirectory
  return $ intoElementaryString path

intoElementaryString v = Elementary (EString v)
intoElementaryInt v = Elementary (EInt v)

-- FFI
randIntFFI :: [Expression] -> IO Expression
randIntFFI [] = return $ Elementary $ (EError "@randInt requires two integer values")
randIntFFI (x:xs) = do
  let (Elementary (EInt v)) = x
  let (Elementary (EInt g)) = getSndArg xs
  res <- randomRIO (v, g)
  return $ Elementary (EInt res)
  where
    getSndArg [] = Elementary (EInt 9999999)
    getSndArg xs = head xs

printFFI, probeFFI, fileFFI, clearScreenFFI :: [Expression] -> IO Expression
clearScreenFFI _ = do
  putStr "\ESC[2J"  -- ANSI escape code to clear the screen
  putStr "\ESC[H"   -- ANSI escape code to move the cursor to the top-left
  hFlush stdout
  return $ Elementary (EVoid)

fileFFI [Elementary (EString value)] = do
    content <- withFile value ReadMode $ \handle -> do
        c <- hGetContents handle
        length c `seq` return c -- Force the content to be read
    return $ Elementary (EString content)
fileFFI [] = return $ Elementary (EError "@file requires at least one argument")


writeFFI ((Elementary (EString fn)):content) = do
  let content' = fmtVexpr content
  writeFile fn content'
  return $ Elementary EVoid

printFFI [] = do
  putStrLn ""
  return $ Elementary EVoid
printFFI [Elementary element] = do
  putStrLn $ showElement element
  return $ Elementary EVoid
printFFI [Chain expressions] = do
 putStrLn $ showHL (Chain expressions)
 return $ Elementary EVoid

printFFI [UnChain expressions] = printFFI expressions

printFFI expressions = do
  mapM (putStrLn . showHL) expressions
  return $ Elementary EVoid

probeFFI [Elementary element] = do
    putStrLn $ showElement element
    return $ Elementary element

probeFFI [expression] = do
  print expression
  return expression

probeFFI expressions = do
  mapM (putStrLn . showElement . extractElement) expressions
  return $ (last expressions)

getInputFFI [] = do
    value <- getLine
    return $ Elementary (EString value)


getInputFFI [Elementary (EString value)] = do
    putStr value
    value <- getLine
    return $ Elementary (EString value)

getInputFFI [expression] = do
    value <- getLine
    return $ Elementary (EString value)

getIntFFI [] = do
  value <- getLine
  return $ Elementary (EInt $ read value)

cpuTimeFFI [] = do
  value <- getSystemTime
  return $ Elementary (EInt $ systemTimeToInt $ value)


systemTimeToInt :: SystemTime -> Int
systemTimeToInt (MkSystemTime secs _) = fromIntegral secs
extractElement :: Expression -> Element
extractElement (Elementary element) = element
extractElement _ = EVoid


fmtVexpr :: [Expression] -> String
fmtVexpr [] = ""
fmtVexpr (x:xs) = work (fmt x) xs
  where work acc [] = acc
        work acc (x:xs) = work (acc <> (fmt x)) xs
        fmt (UnChain xs) = intercalate "\n" (map showHL xs)
        fmt x = showHL x






