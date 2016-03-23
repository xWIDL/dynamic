module Main where

import System.Environment

import Interpret (interpret, showState)
import Parser (parseJS, L(..))

main :: IO ()
main = do
    args <- getArgs
    if (length args > 0) then do
        src <- readFile $ head args
        case parseJS src of
            Right prog -> case interpret (L (-1)) prog of
                (Right (_, s), _)  -> putStrLn $ showState s
                (Left _, logging)  -> putStrLn $ "Failed, log:\n" ++ logging
            Left err -> putStrLn err
        else
            putStrLn "usage: dynamic /PATH/TO/X.js"
