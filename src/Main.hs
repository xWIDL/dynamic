{-|
Module      : Main
-}
module Main (main) where

import System.Environment

import Dynamic.Interpret (interpret, InterpretResult)
import Dynamic.Defs (Abstract(..), InterpretState(..), EnvMap, pprintEnvMap)
import JS.Parser
import JS.AST (prettyPrint)
import APrim (APrim, selectABool, Match(..))
import Common
import Primitive

instance Abstract APrim where
    matchBool prim =
        let (t:f:_) = match (Proxy :: Proxy Bool) [TrueBool, FalseBool] selectABool prim
        in  (t, f)

data Option = ShowLog | NoLog deriving (Eq)

printUsage :: IO ()
printUsage = putStrLn "usage: dynamic -[conn/disconn] -[nolog/showlog] /PATH/TO/X.js"



-- | main
main :: IO ()
main = do
    args <- getArgs
    case args of
        [connArg, logArg, pathArg] -> do
            let mConnArg = parseConnArg connArg
            let mlogArg = parseLogArg logArg
            case (mConnArg, mlogArg) of
                (Just connArg', Just logArg') -> main' connArg' logArg' pathArg
                _ -> printUsage
        _ -> printUsage
    where
        parseConnArg = \case
            "-conn" -> Just True
            "-disconn" -> Just False
            _ -> Nothing
        parseLogArg = \case
            "-showlog" -> Just ShowLog
            "-nolog" -> Just NoLog
            _ -> Nothing

main' :: Bool -> Option -> String -> IO ()
main' connArg logArg file = do
    src <- readFile file
    case parseJS src of
        Right prog -> do
            putStrLn $ "Program:\n" ++ prettyPrint prog ++ "\n"
            ret <- interpret connArg (L (-1)) prog :: IO (InterpretResult L APrim)
            case ret of
                (Right (_, s), _) -> do
                    if logArg == ShowLog
                        then putStrLn $ "\n************** log **************\n\n" ++
                                        (show $ pprintEnvMap (_envMap s :: EnvMap L APrim))
                        else putStrLn $ (show $ pprintEnvMap (_envMap s :: EnvMap L APrim))
                (Left err, logging) ->
                    putStrLn $ "Failed: " ++ err ++ ", log:\n" ++ logging
        Left err -> putStrLn err
