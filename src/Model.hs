module Model where

import AST

import qualified Data.Map as M

data Env = Env {
    _bindings :: M.Map Name Value,
    _store    :: M.Map Ref  Object
} deriving (Show, Eq)

newtype Ref = Ref Int deriving (Show, Eq)

data Value = VPrim Prim | VRef Ref deriving (Show, Eq)

newtype Object = Object (M.Map Name Value) deriving (Show, Eq)
