module Eve.Compiler where

import Control.Monad.Free
import Eve.Data
import Eve.Parser

data OpCodeF next = Refer String next
                 | Constant LispData next
                 | Assign String next
                 | Close [String] (OpCodeF next) next
                 | Def String next
                 | Test (OpCodeF next) (OpCodeF next) next
                 | Halt

instance Functor OpCodeF where
  fmap f (Refer v next) = Refer v (f next)
  fmap f (Constant v next) = Constant v (f next)
  fmap f (Assign n next) = Assign n (f next)
  fmap f (Close xs body next) = Close xs (fmap f body) (f next)
  fmap f (Def n next) = Def n (f next)
  fmap f (Test then' alt next) =  Test (fmap f then') (fmap f alt) (f next)
  fmap _ Halt = Halt

compile' :: Free OpCodeF a -> Ast -> Free OpCodeF a
compile' n (Literal v) = Free (Constant v n)
compile' _ _ = Free Halt

compile :: Ast -> Free OpCodeF ()
compile = compile' (Free Halt)
