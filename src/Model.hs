-- Mode: Execution Model

{-# LANGUAGE TemplateHaskell #-}
module Model where

import AST
import APrim
import Core.Abstract

import qualified Data.Map as M
import Control.Lens

-- Call String: Context Sensitivity

type CallString label = [label]

-- Scope Chain: Closure capturing

data ScopeChain label = TopLevel
                      | Enclosed label -- construction site
                                 label -- closure label
                                 (ScopeChain label) -- father chain
                      deriving (Show, Eq, Ord)

-- Value Model

newtype Ref = Ref Int deriving (Show, Eq, Ord)
incrRef (Ref i) = Ref (i + 1)
initRef = Ref 0

data Value a = VPrim APrim
             | VRef Ref
             | VTop -- FIXME: Can be anything .... which is too coarse
             deriving (Show, Eq)

data Object a = Object (M.Map Name (Value a))
              -- FIXME: Actually, [Function] is also an object ... but we don't model this temporarily
              | OClos (ScopeChain a) (CallString a) [Name] (Stmt a)
              | OTop -- FIXME: ...
              deriving (Show, Eq)

-- Environment

type Bindings a = M.Map Name (Value a)
type Store    a = M.Map Ref  (Object a)

data Env a = Env {
    _bindings :: Bindings a,
    _store    :: Store a,
    _refCount :: Ref
} deriving (Eq)

instance Show a => Show (Env a) where
    show env = "refCount: " ++ show (_refCount env) ++ "\n" ++
               "bindings:\n" ++ concatMap (\(Name x, v) -> x ++ "\t" ++ show v ++ "\n") (M.toList (_bindings env)) ++
               "store:\n" ++ concatMap (\(Ref i, o) -> show i ++ "\t" ++ show o ++ "\n") (M.toList (_store env))

$(makeLenses ''Env)

initEnv :: Env a
initEnv = Env M.empty M.empty initRef

bindValue :: Name -> Value a -> Env a -> Env a
bindValue x v = bindings %~ M.insert x v

storeObj :: Object a -> Env a -> (Env a, Ref)
storeObj o env =
    let ref = _refCount env
    in  (env { _store    = M.insert ref o (_store env),
               _refCount = incrRef ref }, ref)

updateObj :: Ref -> Object a -> Env a -> Env a
updateObj r o env = env { _store = M.insert r o (_store env) }


-- Lattice Model
-- FIXME: Implement the Lattice type-class

unionEnv :: Eq a => Env a -> Env a -> Env a
unionEnv (Env b1 s1 rc1) (Env b2 s2 rc2) =
    Env (b1 `unionBindings` b2) (s1 `unionStore` s2) (rc1 `unionRef` rc2)

unionBindings :: Eq a => Bindings a -> Bindings a -> Bindings a
unionBindings = M.unionWith unionValue

unionStore :: Eq a => Store a -> Store a -> Store a
unionStore    = M.unionWith unionObject

unionValue :: Eq a => Value a -> Value a -> Value a
unionValue (VPrim p1) (VPrim p2) = VPrim $ join p1 p2
unionValue v1 v2 = if v1 == v2 then v1 else VTop -- FIXME: Wow, Magic!

unionObject :: Eq a => Object a -> Object a -> Object a
unionObject o1 o2 = if o1 == o2 then o1 else OTop -- FIXME: Wow, So Magic!

unionRef :: Ref -> Ref -> Ref
unionRef (Ref i1) (Ref i2) = Ref i1 -- FIXME: Seriously?
