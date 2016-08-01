{-|
Module      : Primitive.Bool
Description : Abstract primitive boolean type
-}
module Primitive.Bool (ABool(..)) where

import Core.Abstract
import Core.Domain
import Language.JS.Type
import Language.JS.Platform
import JS.AST

data ABool = FalseBool | TrueBool | TopBool | BotBool deriving (Show, Eq)

instance Lattice ABool where
    top = TopBool
    bot = BotBool

instance Hom Bool ABool where
    hom True = TrueBool
    hom False = FalseBool

instance Domain ABool where
    domainsOf _ =
        let b = Name "b"
        in [ JAssert b (equalsTo b (PBool True))
           , JAssert b (equalsTo b (PBool False)) ]

    reflect _ (isTrue:isFalse:[])
        | isTrue && isFalse     = TopBool
        | isTrue && not isFalse = TrueBool
        | not isTrue && isFalse = FalseBool
        | otherwise             = BotBool

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

