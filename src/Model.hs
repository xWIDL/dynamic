{-# LANGUAGE TemplateHaskell #-}
module Model where

import AST

import qualified Data.Map as M
import Control.Lens

newtype Ref = Ref Int deriving (Show, Eq)

data Value = VPrim Prim | VRef Ref deriving (Show, Eq)

newtype Object = Object (M.Map Name Value) deriving (Show, Eq)

data Env = Env {
    _bindings :: M.Map Name Value,
    _store    :: M.Map Ref  Object
} deriving (Show, Eq)

$(makeLenses ''Env)

initEnv :: Env
initEnv = Env M.empty M.empty

bindValue :: Name -> Value -> Env -> Env
bindValue x v = bindings %~ M.insert x v
