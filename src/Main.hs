module Main where

import Eve.Parser (parseExpr)
import System.Console.Haskeline

parse :: String -> String
parse expr = case parseExpr expr of
  Left e    -> e
  Right ast -> show ast


main :: IO ()
main = runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    line <- getInputLine "eve> "
    case line of
      Nothing -> return ()
      Just ex -> do outputStrLn $ parse ex
                    loop
