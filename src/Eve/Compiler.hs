module Eve.Compiler where

import Control.Monad.Free
import Eve.Data
import Eve.Parser

data OpCodeF next = Refer String next
                  | Constant LispData next
                  | Assign String next
                  | Close [String] (OpCodeF next) next
                  | Def String next
                  | Test next next
                  | Halt deriving Show

type OpCode a = Free OpCodeF a

instance Functor OpCodeF where
  fmap f (Refer v next) = Refer v (f next)
  fmap f (Constant v next) = Constant v (f next)
  fmap f (Assign n next) = Assign n (f next)
  fmap f (Close xs body next) = Close xs (fmap f body) (f next)
  fmap f (Def n next) = Def n (f next)
  fmap f (Test then' alt) =  Test (f then') (f alt)
  fmap _ Halt = Halt

compile' :: Ast -> OpCode a -> OpCode a
compile' (Literal v) n = Free (Constant v n)
compile' (LocalReference s) n = Free (Refer s n)
compile' (Seq xs) n = foldr compile' n xs
compile' (Definition name ast) n = compile' ast (Free $ Def name n)
compile' (Alternative test then' alt) n = compile' test (Free $ Test (compile' then' n) (compile' alt n))
compile' _ _ = Free Halt

compile :: Ast -> OpCode ()
compile ast = compile' ast (Free Halt)
