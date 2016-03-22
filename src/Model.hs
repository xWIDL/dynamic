{-# LANGUAGE TemplateHaskell #-}
module Model where

import AST

import qualified Data.Map as M
import Control.Lens

type Ctx label = [label]

newtype Ref = Ref Int deriving (Show, Eq, Ord)
incrRef (Ref i) = Ref (i + 1)
initRef = Ref 0

-- NOTE: Currently, it is not very abstract. So basically,
--       we are building an interpreter now
data Value a = VPrim Prim
             | VClos a a (Ctx a) [Name] (Stmt a)
             | VRef Ref
             | VTop -- FIXME: Can be anything .... which is too coarse
             deriving (Show, Eq)

data Object a = Object (M.Map Name (Value a))
              | OTop -- FIXME: ...
              deriving (Show, Eq)

data Env a = Env {
    _bindings :: M.Map Name (Value a),
    _store    :: M.Map Ref  (Object a),
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
    Env (M.unionWith unionValue b1 b2) (M.unionWith unionObject s1 s2) (rc1 `unionRef` rc2)

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

applyInfixOp :: Int -> InfixOp -> Int -> Int -- XXX: Think about an "abstract" one
applyInfixOp i1 op i2 = case op of
    OPlus -> i1 + i2
    OSubs -> i1 - i2
    OMult -> i1 * i2
    ODiv  -> i1 `div` i2
