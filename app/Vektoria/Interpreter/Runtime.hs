module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import Vektoria.Lib.Data.Expression

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
  , errors :: [RuntimeError]
  } deriving (Show)


initRuntime::RuntimeState
initRuntime=RuntimeState {scope=initScope, errors=[]}


getEntity :: String -> Runtime (Maybe Entity)
getEntity name = do
    scope <- gets scope
    return $ scope `named` name


addEntity :: String -> (Maybe Metadata, Expression) -> Runtime ()
addEntity name (metadata, expression) = modify $ \s -> s { scope = HashMap.insert name (metadata, expression) (scope s) }


addError :: String -> Runtime ()
addError message = modify $ \s -> s { errors = (errors s) ++ [RuntimeError message] }


initScope :: Scope
initScope = HashMap.fromList testLambdas


named :: Scope -> String -> (Maybe Entity)
scope `named` name = HashMap.lookup name scope


newScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
newScope state newEntities =
    state { scope = HashMap.union (HashMap.fromList newEntities) (scope state)}



addLambda :: Entity
addLambda = (Nothing, Lambda ["a", "b", "c"] (Binary Plus (Reference "a") (Binary Plus (Reference "b") (Reference "c"))))



testLambdas :: [(String, Entity)]
testLambdas = [("add", addLambda)]
