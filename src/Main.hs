{-|
Module      : Main
-}
module Main (main) where

import System.Environment

import Dynamic.Interpret (interpret, InterpretResult)
import Dynamic.Defs (Abstract(..), InterpretState(..), EnvMap, pprintEnvMap)
import JS.Parser
import APrim (APrim, selectABool, Match(..))
import Common
import Primitive

instance Abstract APrim where
    matchBool prim =
        let (t:f:_) = match (Proxy :: Proxy Bool) [TrueBool, FalseBool] selectABool prim
        in  (t, f)

data Option = ShowLog | NoLog deriving (Eq)

printUsage :: IO ()
printUsage = putStrLn "usage: dynamic -[nolog/showlog] /PATH/TO/X.js"

-- | main
main :: IO ()
main = do
    args <- getArgs
    case args of
        [optArg, pathArg] ->
            case optArg of
                "-showlog" -> main' ShowLog pathArg
                "-nolog"   -> main' NoLog pathArg
                _          -> printUsage
        _ -> printUsage

main' :: Option -> String -> IO ()
main' opt file = do
    src <- readFile file
    case parseJS src of
        Right prog -> do
            putStrLn $ "Program:\n" ++ show prog ++ "\n"
            ret <- interpret (L (-1)) prog :: IO (InterpretResult L APrim)
            case ret of
                (Right (_, s), _) -> do
                    if opt == ShowLog
                        then putStrLn $ "\n************** log **************\n\n" ++
                                        (show $ pprintEnvMap (_envMap s :: EnvMap L APrim))
                        else putStrLn $ (show $ pprintEnvMap (_envMap s :: EnvMap L APrim))
                (Left err, logging) ->
                    putStrLn $ "Failed: " ++ err ++ ", log:\n" ++ logging
        Left err -> putStrLn err
