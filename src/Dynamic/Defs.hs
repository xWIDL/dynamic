module Dynamic.Defs (
    EnvMap, WorkList, InterpretState(..),
    Interpret, lookupState, printEnvMap,
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


-- Abstract Primitive Type Class
class (Lattice p, Show p, Reduce p InfixOp, Hom Prim p) => Abstract p where
    -- matchBool is used by path sensitivity analysis
    matchBool :: p -> (Maybe p, Maybe p)

type WorkList label = [Edge label]

type EnvMap label p = M.Map (label, ScopeChain label, CallString label) (Env label p)

data InterpretState label p = InterpretState {
    _envMap   :: EnvMap label p,
    _platPort :: PlatPort
}

type Interpret label p = StateT (InterpretState label p) (ExceptT String (WriterT String IO))

lookupState :: Label l => l -> ScopeChain l -> CallString l -> Interpret l p (Env l p)
lookupState l chain cstr = do
    s <- _envMap <$> get
    case M.lookup (l, chain, cstr) s of
        Nothing -> return initEnv
        Just e  -> return e

printEnvMap :: (Show l, Abstract p) => EnvMap l p -> String
printEnvMap = unlines . map (\((l, sc, cstr), env) -> "--------- EnvMap " ++ show l ++ "----------\n" ++
                                                    "Scope Chain: " ++ show sc ++ "\n" ++
                                                    "Call String: "  ++ show cstr ++ "\n" ++
                                                    "Environment: \n" ++ show env ++ "\n"
                          ) . M.toList
