module Main where

import Data.Functor
import Eve.Parser (parseExpr, Bindings, Binding(..))
import Eve.Compiler
import Eve.VM (exec)
import System.Console.Haskeline
import Eve.Data
import qualified Data.Map.Strict as Map

initialEnv :: [Map.Map String LispData]
initialEnv = [Map.fromList [("+", Function $ Fn2 plus)
                           ,("list", Function $ NAry list)]]

bindings :: Bindings
bindings = (fmap Binding . Map.keys) <$> initialEnv

parse :: String -> String
parse expr = case parseExpr bindings expr of
  Left e    -> e
  Right ast -> show ast

eval :: String -> IO ()
eval expr = case parseExpr bindings expr of
  Left e -> print e
  Right ast -> exec initialEnv (compile ast)

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    line <- getInputLine "eve> "
    case line of
      Nothing -> return ()
      Just ex -> do outputStrLn $ parse ex
                    loop

plus :: LispData -> LispData -> LispData
plus (Number x) (Number y) = Number (x + y)
plus _ _ = error "Plus expects two numbers"

list :: [LispData] -> LispData
list xs = Sexpr $ foldr cons Null xs where
