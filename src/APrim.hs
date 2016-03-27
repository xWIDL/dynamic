-- Abstract Primitive

{-# LANGUAGE TemplateHaskell, PolyKinds, AllowAmbiguousTypes,
             UndecidableInstances, RecordWildCards #-}
module APrim where

import Core.Abstract
import Core.Coercion
import AST
import Primitive.Null
import Primitive.Undefined
import Primitive.Number
import Primitive.Bool
import Primitive.String
import Common

import Control.Lens

data APrim = APrim {
    _aundefined :: AUndefined,
    _anull :: ANull,
    _abool :: ABool,
    _anum  :: ANum,
    _astring :: AString
} deriving (Show, Eq)

$(makeLenses ''APrim)

-- Coerce to String

instance HomLattice AUndefined String AString where
instance HomLattice ANull String AString where
instance HomLattice ABool String AString where
instance HomLattice ANum String AString where
instance HomLattice AString String AString where

instance Coerce APrim String where
    coerce proxy p@(APrim a b c d e) =
        let astr = homlat proxy a `join`
                   homlat proxy b `join`
                   homlat proxy c `join`
                   homlat proxy d `join`
                   homlat proxy e
        in  (astring .~ astr) p

-- Coerce to Number

instance HomLattice ANull Double ANum where
instance HomLattice ABool Double ANum where
instance HomLattice ANum Double ANum where

instance Coerce APrim Double where
    coerce proxy p@(APrim a b c d e) =
        let n = homlat proxy b `join`
                homlat proxy c `join`
                homlat proxy d
        in  (anum .~ n) p

-- Coerce to Bool

instance HomLattice AUndefined Bool ABool where
instance HomLattice AString Bool ABool where
instance HomLattice ANull Bool ABool where
instance HomLattice ABool Bool ABool where
instance HomLattice ANum Bool ABool where

instance Coerce APrim Bool where
    coerce proxy p@(APrim a b c d e) =
        let b = homlat proxy a `join`
                homlat proxy b `join`
                homlat proxy c `join`
                homlat proxy d `join`
                homlat proxy e
        in  (abool .~ b) p

-- Lattice product
instance Lattice APrim where
    meet p1 p2 = APrim (_aundefined p1 `meet` _aundefined p2)
                       (_anull p1      `meet` _anull p2)
                       (_abool p1      `meet` _abool p2)
                       (_anum p1       `meet` _anum p2)
                       (_astring p1    `meet` _astring p2)

    join p1 p2 = APrim (_aundefined p1 `join` _aundefined p2)
                       (_anull p1      `join` _anull p2)
                       (_abool p1      `join` _abool p2)
                       (_anum p1       `join` _anum p2)
                       (_astring p1    `join` _astring p2)

    top = APrim top top top top top

    bot = APrim bot bot bot bot bot

instance Hom Prim APrim where
    hom (PrimNum n)   = anum .~ (hom n) $ bot
    hom (PrimStr n)   = astring .~ (hom n) $ bot
    hom (PrimBool n)  = abool .~ (hom n) $ bot
    hom PrimNull      = anull .~ (hom PrimNull) $ bot
    hom PrimUndefined = aundefined .~ (hom PrimUndefined) $ bot

-- Reduction Framework
instance Reduce APrim InfixOp where
    reduce op p1 p2 =
        let p1n = coerce (Proxy :: Proxy Double) p1
            p2n = coerce (Proxy :: Proxy Double) p2
            p1s = coerce (Proxy :: Proxy String) p1
            p2s = coerce (Proxy :: Proxy String) p2
            n   = reduce op (_anum p1n) (_anum p2n)
            s   = reduce op (_astring p1n) (_astring p2n)
        in  (anum .~ n) bot `join` (astring .~ s) bot


-- Path Sentivitity Framework
class Match a where
    match :: Coerce APrim aconc => Proxy aconc -> [a] -> Lens' APrim a -> APrim -> [Maybe APrim]

instance (Hom a [AUndefined], Hom a [ANull],
          Hom a [AString], Hom a [ANum], Hom a [ABool]) => Match a where
    match proxy as lens p@APrim{..} = flip map as $ \field ->
        let tester = coerce proxy p `meet` ((lens .~ field) bot)
        in  if tester == bot
                then Nothing
                else (Just (APrim (_aundefined `meet'` field)
                                  (_anull `meet'` field)
                                  (_abool `meet'` field)
                                  (_anum `meet'` field)
                                  (_astring `meet'` field)))

instance Hom ABool [ANull] where
    hom TopBool = [top]
    hom BotBool = [bot]
    hom TrueBool = []
    hom FalseBool = [Null]

instance Hom ABool [AUndefined] where
    hom TopBool = [top]
    hom BotBool = [bot]
    hom TrueBool = []
    hom FalseBool = [Undefined]

instance Hom ABool [ANum] where
    hom TopBool = [top]
    hom BotBool = [bot]
    hom TrueBool = [NegNum, PosNum]
    hom FalseBool = [ZeroNum]

instance Hom ABool [AString] where
    hom TopBool = [top]
    hom BotBool = [bot]
    hom TrueBool = [NonEmptyString]
    hom FalseBool = [EmptyString]

matchBool :: APrim -> [Maybe APrim]
matchBool = match (Proxy :: Proxy Bool) [TrueBool, FalseBool] abool
