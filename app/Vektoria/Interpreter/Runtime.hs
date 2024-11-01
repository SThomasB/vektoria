module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Vektoria.Lib.Data.Expression
import Vektoria.Lib.Data.Element
import Data.Time.Clock.System
import Data.Unique
import Data.Char (ord)
import System.Random (randomRIO)
import System.Directory
import System.Environment
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

data RuntimeState = RuntimeState
  { scope :: Scope
  , ffi :: Scope
  , errors :: [RuntimeError]
  }
instance Show RuntimeState where
  show s = (show (scope s ))

initRuntime :: RuntimeState
initRuntime = RuntimeState {scope=initScope, ffi=initFFI, errors=[]}

getEntity, getForeign :: String -> Runtime (Maybe Entity)
getEntity name = do
    scope <- gets scope
    return $ scope `named` name

getForeign name = do
  foreignEntities <- gets ffi
  return $ foreignEntities `named` name

addEntity :: String -> (Maybe Metadata, Expression) -> Runtime ()
addEntity name (metadata, expression) = modify $ \s -> s { scope = HashMap.insert name (metadata, expression) (scope s) }

addError :: String -> Runtime ()
addError message = modify $ \s -> s { errors = (errors s) ++ [RuntimeError message] }

initScope, initFFI :: Scope
initScope = HashMap.fromList []
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
                 ,("randInt", (Nothing, IOAction randIntFFI))
                 ,("folder", (Nothing, IOAction folderFFI))
                 ,("args", (Nothing, IOAction argsFFI))
                 ,("len", (Nothing, Primitive lenFFI))
                 ,("ascii", (Nothing, Primitive asciiFFI))
                 ]

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

intoElementaryString v = Elementary (EString v)

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

printFFI, probeFFI :: [Expression] -> IO Expression
fileFFI [] = return $ Elementary (EError "@file requires at least one argument")
fileFFI [Elementary (EString value)] = do
 content <- readFile value
 return $ Elementary (EString content)
printFFI [] = do
  putStrLn ""
  return $ Elementary EVoid
printFFI [Elementary element] = do
  putStrLn $ showElement element
  return $ Elementary EVoid
printFFI [Chain expressions] = do
 putStrLn $ showHL (Chain expressions)
 return $ Elementary EVoid
printFFI expressions = do
  mapM (putStrLn . showElement . extractElement) expressions
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
