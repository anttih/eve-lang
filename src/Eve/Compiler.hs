module Eve.Compiler where

import Data.Functor
import Control.Monad.Free
import Eve.Data hiding (Function)
import Eve.Parser

compile' :: Ast -> OpCode -> OpCode
compile' (Literal v) n = Free (Constant v n)
compile' (LocalReference s) n = Free (Refer s n)
compile' (Seq xs) n = foldr compile' n xs
compile' (Definition name ast) n = compile' ast (Free $ Def name n)
compile' (Function params body) n = Free $ Close params' body' n where
  params' = (\(Binding s) -> s) <$> params
  body' = compile' body (Free Return)
compile' (Alternative test then' alt) n = compile' test (Free $ Test (compile' then' n) (compile' alt n))
compile' (Application f args) n = funCall args (compile' f (Free Apply)) n where
compile' _ _ = Free Halt

funCall :: [Ast] -> OpCode -> OpCode -> OpCode
funCall [] c' n = Free $ Frame n c'
funCall (x:xs) c' n = funCall xs (compile' x (Free $ Argument c')) n

compile :: Ast -> OpCode
compile ast = compile' ast (Free Halt)
