{-# LANGUAGE TemplateHaskell #-}
module Model where

import AST

import qualified Data.Map as M
import Control.Lens

-- Scope Chain

data ScopeChain label = TopLevel
                      | Enclosed label -- construction site
                                 label -- closure label
                                 (ScopeChain label) -- father chain
                      deriving (Show, Eq, Ord)

-- Memory Model

newtype Ref = Ref Int deriving (Show, Eq, Ord)
incrRef (Ref i) = Ref (i + 1)
initRef = Ref 0

-- NOTE: Currently, it is not very abstract. So basically,
--       we are building an interpreter now
data Value a = VPrim Prim
             | VRef Ref
             | VTop -- FIXME: Can be anything .... which is too coarse
             deriving (Show, Eq)

--- XXX: Currently, we can't express the side-effects caused by on-heap allocation
--- wondering how to do it correctly.
data Object a = Object (M.Map Name (Value a))
              -- FIXME: Actually, [Function] is also an object ... but we don't model this temporarily
              | OClos (ScopeChain a) [Name] (Stmt a)
              | OTop -- FIXME: ...
              deriving (Show, Eq)

type Bindings a = M.Map Name (Value a)
type Store    a = M.Map Ref  (Object a)

data Env a = Env {
    _bindings :: Bindings a,
    _store    :: Store a,
    _refCount :: Ref
} deriving (Eq)

instance Show a => Show (Env a) where
    show env = "bindings:\n" ++ concatMap (\(Name x, v) -> x ++ "\t" ++ show v ++ "\n") (M.toList (_bindings env)) ++
               "store:\n" ++ concatMap (\(Ref i, o) -> show i ++ "\t" ++ show o ++ "\n") (M.toList (_store env))

$(makeLenses ''Env)

initEnv :: Env a
initEnv = Env M.empty M.empty initRef

-- NOTE: Where "lattice" comes into play
unionEnv :: Eq a => Env a -> Env a -> Env a
unionEnv (Env b1 s1 rc1) (Env b2 s2 rc2) =
    Env (b1 `unionBindings` b2) (s1 `unionStore` s2) (rc1 `unionRef` rc2)

unionBindings :: Eq a => Bindings a -> Bindings a -> Bindings a
unionBindings = M.unionWith unionValue

unionStore :: Eq a => Store a -> Store a -> Store a
unionStore    = M.unionWith unionObject

unionValue :: Eq a => Value a -> Value a -> Value a
unionValue (VPrim PrimUndefined) v = v
unionValue (VPrim PrimNull) v = v
unionValue v (VPrim PrimUndefined) = v
unionValue v (VPrim PrimNull) = v
unionValue v1 v2 = if v1 == v2 then v1 else VTop -- FIXME: Wow, Magic!

unionObject :: Eq a => Object a -> Object a -> Object a
unionObject o1 o2 = if o1 == o2 then o1 else OTop -- FIXME: Wow, So Magic!

unionRef :: Ref -> Ref -> Ref
unionRef (Ref i1) (Ref i2) = Ref (max i1 i2)

bindValue :: Name -> Value a -> Env a -> Env a
bindValue x v = bindings %~ M.insert x v

storeObj :: Object a -> Env a -> (Env a, Ref)
storeObj o env =
    let ref = _refCount env
    in  (env { _store    = M.insert ref o (_store env),
               _refCount = incrRef ref }, ref)

-- XXX: Think about an "abstract" one
-- XXX: Consider more coersions
applyInfixOp :: Prim -> InfixOp -> Prim -> Prim
applyInfixOp (PrimNum x1) op (PrimNum x2) = PrimNum $ case op of
    OPlus -> x1 + x2
    OSubs -> x1 - x2
    OMult -> x1 * x2
    ODiv  -> x1 / x2
applyInfixOp _ _ _ = PrimUndefined

reachableFrom :: M.Map Ref (Object a) -> Value a -> M.Map Ref (Object a)
reachableFrom _ (VPrim _) = M.empty
reachableFrom m (VRef r) =
    case M.lookup r m of
        Nothing -> M.empty -- FIXME: Is this sound?
        Just o  -> case o of
            Object dict -> foldr M.union M.empty (map (reachableFrom m) (M.elems dict))
            OClos _ _ _ -> M.singleton r o
            OTop -> m -- XXX: Wow, Magic!
reachableFrom m VTop     = m -- XXX: Wow, Magic!
