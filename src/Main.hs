module Main where

import System.Environment

import Interpret
import Parser
import APrim

instance Abstract APrim where

main :: IO ()
main = do
    args <- getArgs
    if (length args > 0) then main' (head args)
        else
            putStrLn "usage: dynamic /PATH/TO/X.js"

main' :: String -> IO ()
main' file = do
    src <- readFile file
    case parseJS src of
        Right prog -> do
            putStrLn $ "Program:\n" ++ show prog ++ "\n"
            case interpret (L (-1)) prog of
                (Right (_, s), _)  -> putStrLn $ showState (s :: InterpretState L APrim)
                (Left err, logging) ->
                    putStrLn $ "Failed: " ++ err ++ ", log:\n" ++ logging
        Left err -> putStrLn err
