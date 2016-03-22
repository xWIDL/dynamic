{-# LANGUAGE TemplateHaskell #-}
module Model where

import AST

import qualified Data.Map as M
import Control.Lens

newtype Ref = Ref Int deriving (Show, Eq, Ord)
incrRef (Ref i) = Ref (i + 1)
initRef = Ref 0

-- NOTE: Currently, it is not very abstract. So basically,
--       we are building an interpreter now
data Value a = VPrim Prim
             | VClos [Name] (Stmt a)
             | VRef Ref
             deriving (Show, Eq)

newtype Object a = Object (M.Map Name (Value a)) deriving (Show, Eq)

data Env a = Env {
    _bindings :: M.Map Name (Value a),
    _store    :: M.Map Ref  (Object a),
    _refCount :: Ref
} deriving (Show, Eq)

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
