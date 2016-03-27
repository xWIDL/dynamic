-- Abstract Primitive

{-# LANGUAGE TemplateHaskell, PolyKinds, AllowAmbiguousTypes,
             UndecidableInstances, ScopedTypeVariables,
             RecordWildCards, RankNTypes #-}
module APrim where

import Core.Abstract
import AST
import Primitive.Null
import Primitive.Undefined
import Primitive.Number
import Primitive.Bool
import Primitive.String


import Control.Lens

data APrim = APrim {
    _aundefined :: AUndefined,
    _anull :: ANull,
    _abool :: ABool,
    _anum  :: ANum,
    _astring :: AString
} deriving (Show, Eq)

$(makeLenses ''APrim)

-- Coercion Framework

class (Lattice a, Lattice b, Hom a bconc, Hom bconc b) => HomLattice a (bconc :: *) b where
    homlat :: Proxy bconc -> a -> b

    homlat _ a | a == top  = top
               | a == bot  = bot
               | otherwise = hom (hom a :: bconc) :: b

data Proxy t = Proxy

class Coerce aconc where
    coerce :: Proxy aconc -> APrim -> APrim

-- Coerce to String

instance HomLattice AUndefined String AString where
instance HomLattice ANull String AString where
instance HomLattice ABool String AString where
instance HomLattice ANum String AString where
instance HomLattice AString String AString where

instance Coerce String where
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

instance Coerce Double where
    coerce proxy p@(APrim a b c d e) =
        let n = homlat proxy b `join`
                homlat proxy c `join`
                homlat proxy d
        in  (anum .~ n) p

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
    match :: Coerce aconc => Proxy aconc -> [a] -> Lens' APrim a -> APrim -> [Maybe APrim]

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
        where
            meet' :: forall a b. Lattice b => Hom a [b] => b -> a -> b
            meet' b a = let botb = bot :: b
                        in  foldr (\b' ret -> (b `meet` b') `join` ret) botb (hom a :: [b])
