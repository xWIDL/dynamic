{-|
Module      : Dynamic.Defs
Description : Definitions used in interpeter
-}

module Dynamic.Defs (
    EnvMap, WorkList, InterpretState(..),
    Interpret, lookupEnv, printEnvMap,
    Abstract(..)
) where

import Core.Abstract (Lattice, Reduce, Hom)
import Core.Flow (Edge, Label)
import JS.AST (InfixOp)
import JS.Type (Prim)
import JS.Model (Env, initEnv, ScopeChain, CallString)
import JS.Platform (PlatPort)

import qualified Data.Map as M
import Control.Monad.State (StateT, get)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT)


-- | Abstract Primitive Type Class
class (Lattice p, Show p, Reduce p InfixOp, Hom Prim p) => Abstract p where
    -- | Used by path sensitivity analysis
    matchBool :: p -> (Maybe p, Maybe p)

-- | List of edges to update
type WorkList label = [Edge label]

-- | Mapping of context identifier to environment
type EnvMap label p = M.Map (label, ScopeChain label, CallString label) (Env label p)

-- | Interpreter state
data InterpretState label p = InterpretState {
    _envMap   :: EnvMap label p,
    _platPort :: PlatPort -- ^ Platform communication port
}

-- | Interpreter monad stack
type Interpret label p = StateT (InterpretState label p) (ExceptT String (WriterT String IO))

-- | lookup environment indexed by block label, scope chain and call string
lookupEnv :: Label l => l -> ScopeChain l -> CallString l -> Interpret l p (Env l p)
lookupEnv l chain cstr = do
    s <- _envMap <$> get
    case M.lookup (l, chain, cstr) s of
        Nothing -> return initEnv
        Just e  -> return e

-- | Pretty print EnvMap
printEnvMap :: (Show l, Abstract p) => EnvMap l p -> String
printEnvMap = unlines . map (\((l, sc, cstr), env) -> "--------- EnvMap " ++ show l ++ "----------\n" ++
                                                    "Scope Chain: " ++ show sc ++ "\n" ++
                                                    "Call String: "  ++ show cstr ++ "\n" ++
                                                    "Environment: \n" ++ show env ++ "\n"
                          ) . M.toList
