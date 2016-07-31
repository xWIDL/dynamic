{-|
Module      : Primitive.Undefined
Description : Abstract primitive undefined type
-}

module Primitive.Undefined (AUndefined(..)) where

import Core.Abstract
import Language.JS.Type
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
