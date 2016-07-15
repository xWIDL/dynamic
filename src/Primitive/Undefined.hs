-- Abstract Undefined
module Primitive.Undefined where

import Core.Abstract
import JS.Type
import JS.AST

data AUndefined = Undefined | UndefinedBot deriving (Show, Eq)

instance Lattice AUndefined where
    top = Undefined
    bot = UndefinedBot

instance Hom Prim AUndefined where
    hom PUndefined = Undefined

instance Reduce AUndefined InfixOp where
    reduce _ _ _ = top

instance Hom AUndefined String where
    hom Undefined = "undefined"

instance Hom AUndefined Bool where
    hom Undefined = False
