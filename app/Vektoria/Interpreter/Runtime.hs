module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Vektoria.Lib.Data.Entity
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
type Runtime a = StateT RuntimeState IO a
type EntityMap = HashMap.HashMap String Entity



data RuntimeError = RuntimeError {
    message :: String
} deriving (Show)

data RuntimeState = RuntimeState
  { entities :: EntityMap
  , errors :: [RuntimeError]
  } deriving (Show)


initRuntime::RuntimeState
initRuntime = RuntimeState {entities=initEntityMap, errors=[]}


getEntity :: String -> Runtime (Maybe Entity)
getEntity name = do
    entities <- gets entities
    return $ entities `named` name


addEntity :: String -> Entity -> Runtime ()
addEntity name value = modify $ \s -> s { entities = HashMap.insert name value (entities s) }

addError :: String -> Runtime ()
addError message = modify $ \s -> s { errors = errors s ++ [RuntimeError message] }


initEntityMap :: EntityMap
initEntityMap = HashMap.fromList testCallables


named :: EntityMap -> String -> (Maybe Entity)
entities `named` name = HashMap.lookup name entities


newScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
newScope state newEntities =
    state { entities = HashMap.union (HashMap.fromList newEntities) (entities state)}
