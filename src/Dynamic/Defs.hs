{-|
Module      : Dynamic.Defs
Description : Definitions used in interpeter
-}

module Dynamic.Defs (
    EnvMap, WorkList, InterpretState(..),
    Interpret, lookupEnv, pprintEnvMap,
    Abstract(..), Cursor(..), pprintCursor,
    tryForwardCursor, appendNext, changeCursor,
    getEdgePair
) where

import Core.Abstract (Lattice, Reduce, Hom)
import Core.Flow (Edge(..), Label)
import JS.AST (InfixOp, Stmt)
import Language.JS.Type (Prim)
import Primitive
import JS.Model (Env, initEnv, ScopeChain, CallString)
import Language.JS.Platform (PlatPort)

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT)
import Text.PrettyPrint.Leijen hiding ((<$>))

-- | Abstract Primitive Type Class
class (Lattice p, Show p, Reduce p InfixOp, Hom Prim p,
       Hom ABool p, Hom ANum p, Hom AString p) => Abstract p where
    -- | Used by path sensitivity analysis
    matchBool :: p -> (Maybe p, Maybe p)

-- | List of edges to update
type WorkList label = [Edge label]

-- | Mapping of context identifier to environment
type EnvMap label p = M.Map (label, ScopeChain label, CallString label) (Env label p)

-- | Cursor of current analysis
data Cursor label = Cursor {
    _chain :: ScopeChain label,
    _cstr  :: CallString label,
    _edge  :: Edge label,
    _next  :: WorkList label
} deriving (Show)

-- | Interpreter state
data InterpretState label p = InterpretState {
    _envMap    :: EnvMap label p,
    _platPort  :: Maybe PlatPort, -- ^ Platform communication port,
    _labelDict :: M.Map label (Stmt label),
    _flows     :: S.Set (Edge label),
    _cursor    :: Cursor label,
    _connected :: Bool
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
pprintEnvMap :: (Show l, Abstract p) => EnvMap l p -> Doc
pprintEnvMap m = vsep $ map pprintEnvEntry (M.toList m)
    where
    pprintEnvEntry ((l, sc, cstr), env) = vsep
        [ text "--------- EnvMap " <> text (show l) <> text "----------",
          text "Scope Chain:"  <+> text (show sc),
          text "Call String:"  <+> text (show cstr),
          text "Environment:"  <>  line <> text (show env) ]

pprintCursor :: (Show label) => Cursor label -> Doc
pprintCursor cursor = vsep
    [ text "========== State =========",
      text "|| chain:" <+> text (show $ _chain cursor),
      text "|| cstr:" <+> text (show $ _cstr cursor),
      text "|| edge:" <+> text (show $ _edge cursor),
      text "|| next:" <+> text (show $ _next cursor) ]

tryForwardCursor :: Interpret label p Bool
tryForwardCursor = do
    wl <- (_next . _cursor) <$> get
    case wl of
        [] -> return False -- failed
        w:wl' -> do
            modify (\s -> s { _cursor = (_cursor s) { _edge = w, _next = wl' } })
            return True

appendNext :: WorkList label -> Interpret label p ()
appendNext wl =
    modify $ \s ->
    let cursor = _cursor s
        cursor' = cursor { _next = (_next cursor) ++ wl }
    in s { _cursor = cursor' }

changeCursor :: Cursor label -> Interpret label p ()
changeCursor cursor = modify $ \s -> s { _cursor = cursor }

getEdgePair :: Cursor label -> (label, label)
getEdgePair cursor =
    case (_edge cursor) of
        Edge p -> p
        ExitTry p _ -> p
        EnterTry p _ -> p
