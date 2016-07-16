{-|
Module      : Primitive.Bool
Description : Abstract primitive boolean type
-}
module Primitive.Bool (ABool(..)) where

import Core.Abstract
import JS.Type
import JS.AST

data ABool = FalseBool | TrueBool | TopBool | BotBool deriving (Show, Eq)

instance Lattice ABool where
    top = TopBool
    bot = BotBool

instance Hom Bool ABool where
    hom True = TrueBool
    hom False = FalseBool

instance Hom ABool Bool where
    hom TrueBool  = True
    hom FalseBool = False

instance Hom ABool Prim where
    hom b = PBool (hom b)

instance Reduce ABool InfixOp where
    reduce _ _ _ = top -- FIXME

instance Hom ABool String where
    hom TrueBool  = show (hom TrueBool :: Bool)
    hom FalseBool = show (hom FalseBool :: Bool)

instance Hom ABool Double where
    hom TrueBool  = 1.0
    hom FalseBool = 0.0
