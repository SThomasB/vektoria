module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Vektoria.Lib.Data.Expression
import Vektoria.Lib.Data.Element
import System.CPUTime
import Data.Unique
type Runtime a = StateT RuntimeState IO a
type Scope = HashMap.HashMap String Entity
type Closures = HashMap.HashMap Unique Scope


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
  , closures :: Closures
  , errors :: [RuntimeError]
  }
instance Show RuntimeState where
  show s = (show (scope s ))


initRuntime::RuntimeState
initRuntime=RuntimeState {closures=initClosures, scope=initScope, ffi=initFFI, errors=[]}

initClosures :: Closures
initClosures = HashMap.empty
createClosure :: Runtime ()
createClosure = do
  id <- liftIO $ newUnique
  modify $ \s -> s {closures=HashMap.insert id HashMap.empty (closures s)}
  where

addToClosure :: Unique -> String -> Entity -> Runtime ()
addToClosure id name entity = do
  closures' <- gets closures
  closure' <- case HashMap.lookup id closures' of
    Just closure -> return $ HashMap.insert name entity closure
    Nothing -> error "No such closure id exists"
  modify $ \s -> s {closures=HashMap.insert id closure' (closures')}

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
initScope = HashMap.fromList testLambdas
initFFI = HashMap.fromList foreignFunctions


named :: Scope -> String -> (Maybe Entity)
scope `named` name = HashMap.lookup name scope


newScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
newScope state newEntities =
    state { scope = HashMap.union (HashMap.fromList newEntities) (scope state)}



addLambda :: Entity
addLambda = (Nothing, Lambda Nothing ["a", "b", "c"] (Binary Plus (Reference "a") (Binary Plus (Reference "b") (Reference "c"))))



testLambdas :: [(String, Entity)]
testLambdas = [("add", addLambda)]

foreignFunctions :: [(String, Entity)]
foreignFunctions =
                 [("print", (Nothing, IOAction printFFI))
                 ,("probe", (Nothing, IOAction probeFFI))
                 ,("user" , (Nothing, IOAction getInputFFI))
                 ,("cpuTime", (Nothing, IOAction cpuTimeFFI))
                 ,("file", (Nothing, IOAction fileFFI))
                 ]

-- FFI

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

cpuTimeFFI [] = do
  value <- getCPUTime
  return $ Elementary (EInt $ fromIntegral value)


extractElement :: Expression -> Element
extractElement (Elementary element) = element
extractElement _ = EVoid
