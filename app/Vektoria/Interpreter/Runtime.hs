module Vektoria.Interpreter.Runtime (module Vektoria.Interpreter.Runtime,
  module Control.Monad.State) where
import Vektoria.Lib.Data.Statement
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
type Runtime a = StateT RuntimeState IO a
type EntityMap = HashMap.HashMap String Entity



data RuntimeState = RuntimeState
  { entities :: EntityMap
  , errors :: [Element]
  } deriving (Show)


initRuntime::RuntimeState
initRuntime = RuntimeState {entities=initEntityMap, errors=[]}


getEntity :: String -> Runtime (Maybe Entity)
getEntity name = do
    entities <- gets entities
    return $ entities `named` name


addEntity :: String -> Entity -> Runtime ()
addEntity name value = modify $ \s -> s { entities = HashMap.insert name value (entities s) }

addError :: Element -> Runtime ()
addError err = modify $ \s -> s { errors = errors s ++ [err] }


initEntityMap :: EntityMap
initEntityMap = HashMap.fromList [
  ("add", Callable ["a", "b", "c"] (Binary Plus (Ref "a") (Binary Plus (Ref "b") (Ref "c"))))]

named :: EntityMap -> String -> (Maybe Entity)
entities `named` name = HashMap.lookup name entities


makeScope :: RuntimeState -> [(String, Entity)] -> RuntimeState
makeScope state newEntities =
    state { entities = HashMap.union (HashMap.fromList newEntities) (entities state)}
