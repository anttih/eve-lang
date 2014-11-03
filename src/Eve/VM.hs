module Eve.VM (
  exec
  ) where

import Control.Monad.Free
import Control.Monad.State.Strict
import Control.Exception.Base

import Data.Maybe
import Data.List
import Data.Functor
import qualified Data.Map.Strict as Map

import Eve.Data
import Eve.Compiler

type VM = StateT (LispData, [Map.Map String LispData]) IO ()

run :: Free OpCodeF a -> VM
run (Free (Constant v n)) = modify (\(_, env) -> (v, env)) >> run n
run (Free (Refer v n))    = modify (\(_, env) -> (refer v env, env)) >> run n
run (Free (Def name n))   = modify updateFrame >> run n where
  updateFrame (v, [])       = (v, [Map.singleton name v])
  updateFrame (v, frame:xs) = (v, Map.insert name v frame : xs)
run (Free Halt)           = get >>= liftIO . print . fst
run (Pure _)              = liftIO (throwIO (userError "Fail"))
run _                     = liftIO (throwIO (userError "Not implemented"))

-- This is unsafe, but using this for LocalReferences should always succeed
refer :: String -> [Map.Map String LispData] -> LispData
refer k env = fromJust $ join $ find local (Map.lookup k <$> env) where
  local (Just _) = True
  local Nothing = False

exec :: Free OpCodeF () -> IO ()
exec op = void $ runStateT (run op) (Sexpr Null, [])
