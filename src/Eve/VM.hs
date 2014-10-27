module Eve.VM (
  exec
  ) where

import Control.Monad.Free
import Control.Monad.State.Strict
import Control.Exception.Base
import Eve.Data
import Eve.Compiler

type VM = StateT (LispData, [[String]]) IO ()

run :: Free OpCodeF a -> VM
run (Free (Constant v n)) = modify (\(_, env) -> (v, env)) >> run n
run (Free Halt)           = do
  (a, _) <- get
  liftIO (print a)
run (Pure _)              = liftIO (throwIO (userError "Fail"))

exec :: Free OpCodeF () -> IO ()
exec op = void $ runStateT (run op) (Sexpr Null, [])
