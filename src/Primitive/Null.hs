-- Abstract Null
module Primitive.Null (ANull(..)) where

import Core.Abstract
import JS.Type
import JS.AST

data ANull = Null | NullBot deriving (Show, Eq)

instance Lattice ANull where
    top = Null
    bot = NullBot

instance Hom Prim ANull where
    hom PNull = Null

instance Hom ANull Prim where
    hom Null = PNull

instance Reduce ANull InfixOp where
    reduce _ _ _ = top

instance Hom ANull String where
    hom Null = "null"

instance Hom ANull Double where
    hom Null = 0.0

instance Hom ANull Bool where
    hom Null = False
