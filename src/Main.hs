module Main where

import Eve.Parser (parseExpr)
import Eve.Compiler
import Eve.VM (exec)
import System.Console.Haskeline

parse :: String -> String
parse expr = case parseExpr expr of
  Left e    -> e
  Right ast -> show ast


eval :: String -> IO ()
eval expr = case parseExpr expr of
  Left e -> print e
  Right ast -> exec (compile ast)

main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    line <- getInputLine "eve> "
    case line of
      Nothing -> return ()
      Just ex -> do outputStrLn $ parse ex
                    loop
