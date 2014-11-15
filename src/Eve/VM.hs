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

data StackFrame = StackFrame [Map.Map String LispData] [LispData] StackFrame | EmptyStack

type VM = StateT (LispData, [Map.Map String LispData], [LispData], StackFrame) IO ()

run :: OpCode () -> VM
run (Free (Constant v n)) = modify (\(_, env, f, s) -> (v, env, f, s)) >> run n
run (Free (Refer v n))    = modify (\(_, env, f, s) -> (refer v env, env, f, s)) >> run n
run (Free (Def name n))   = modify updateFrame >> run n where
  updateFrame (v, [], f, s)       = (v, [Map.singleton name v], f, s)
  updateFrame (v, frame:xs, f, s) = (v, Map.insert name v frame : xs, f, s)
run (Free (Test then' alt)) = do
  test <- (\(a, _, _, _) -> a) <$> get
  case test of
    (LispBool a) -> run (if a then then' else alt)
    _ -> liftIO (throwIO (userError "Non-boolean value in if expression"))
run (Free (Argument n))   = modify (\(a, env, f, s) -> (a, env, a : f, s)) >> run n
run (Free Apply)          = modify (\(a, env, f, s) -> (apply a f, env, f, s)) >> run (Free Return)
run (Free (Frame ret n))  = modify (\(a, env, f, s) -> (a, env, f, StackFrame env f s)) >> run n >> run ret
run (Free Return)         = modify (\(a, env, f, s) -> case s of
                                                       (StackFrame env' f' s') -> (a, env', f', s')
                                                       EmptyStack              -> (a, env, f, s))
run (Free (Close names body n)) = modify (\(_, env, f, s) -> (Closure names body env, env, f, s)) >> run n
run (Free Halt)           = get >>= liftIO . print . (\(a, _, _, _) -> a)
run (Pure _)              = liftIO (throwIO (userError "Fail"))
run _                     = liftIO (throwIO (userError "Not implemented"))

apply :: LispData -> [LispData] -> LispData
apply (Function (NAry f)) xs = f xs
apply (Function (Fn1 f)) (x1:[]) = f x1
apply (Function (Fn2 f)) (x1:x2:[]) = f x1 x2
apply (Function (Fn3 f)) (x1:x2:x3:[]) = f x1 x2 x3
apply (Function (Fn4 f)) (x1:x2:x3:x4:[]) = f x1 x2 x3 x4
apply (Function (Fn5 f)) (x1:x2:x3:x4:x5:[]) = f x1 x2 x3 x4 x5
apply (Function (Fn6 f)) (x1:x2:x3:x4:x5:x6:[]) = f x1 x2 x3 x4 x5 x6
apply (Function (Fn7 f)) (x1:x2:x3:x4:x5:x6:x7:[]) = f x1 x2 x3 x4 x5 x6 x7
apply _ _ = error "Cannot apply"

-- This is unsafe, but using this for LocalReferences should always succeed
refer :: String -> [Map.Map String LispData] -> LispData
refer k env = fromJust $ join $ find local (Map.lookup k <$> env) where
  local (Just _) = True
  local Nothing = False

exec :: [Map.Map String LispData] -> Free OpCodeF () -> IO ()
exec env op = void $ runStateT (run op) (Sexpr Null, env, [], EmptyStack)
