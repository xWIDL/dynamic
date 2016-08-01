{-|
Module      : APrim
Description : Abstract primitive type
-}

{-# LANGUAGE TemplateHaskell, PolyKinds, AllowAmbiguousTypes, UndecidableInstances, RecordWildCards #-}

module APrim (APrim, selectABool, Match(..)) where

import Core.Abstract
import Core.Coercion
import JS.AST
import Language.JS.Type
import Common
import Primitive
import Control.Lens (makeLenses, Lens', (.~))

-- | Abstract primitive types as a product of sub-types
data APrim = APrim {
    _aundefined :: AUndefined,
    _anull :: ANull,
    _abool :: ABool,
    _anum  :: ANum,
    _astring :: AString
} deriving (Show, Eq)

$(makeLenses ''APrim)

-- | Select boolean part of APrim
selectABool :: Lens' APrim ABool
selectABool = abool

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
    coerce proxy p@(APrim _a b c d _e) =
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
        let bool = homlat proxy a `join`
                homlat proxy b `join`
                homlat proxy c `join`
                homlat proxy d `join`
                homlat proxy e
        in  (abool .~ bool) p

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
    hom (PInt n)    = anum .~ (hom n) $ bot
    hom (PDouble n) = anum .~ (hom n) $ bot -- Funny
    hom (PString n) = astring .~ (hom n) $ bot
    hom (PBool n)   = abool .~ (hom n) $ bot
    hom PNull       = anull .~ (hom PNull) $ bot
    hom PUndefined  = aundefined .~ (hom PUndefined) $ bot

instance Hom ANum APrim where
    hom x = anum .~ x $ bot

instance Hom AString APrim where
    hom x = astring .~ x $ bot

instance Hom ABool APrim where
    hom x = abool .~ x $ bot

instance Hom APrim Prim where
    hom (APrim _ p2 p3 p4 p5)
      | p3 == bot && p4 == bot && p5 == bot = hom p2
      | p4 == bot && p5 == bot && p2 == bot = hom p3
      | p2 == bot && p3 == bot && p5 == bot = hom p4
      | p2 == bot && p3 == bot && p4 == bot = hom p5
      | otherwise = PNull
    -- hom (PrimStr n)   = astring .~ (hom n) $ bot
    -- hom (PrimBool n)  = abool .~ (hom n) $ bot
    -- hom PNull      = anull .~ (hom PNull) $ bot
    -- hom PrimUndefined = aundefined .~ (hom PrimUndefined) $ bot

-- Reduction Framework
instance Reduce APrim InfixOp where
    reduce op p1 p2 =
        let p1n = coerce (Proxy :: Proxy Double) p1
            p2n = coerce (Proxy :: Proxy Double) p2
            p1s = coerce (Proxy :: Proxy String) p1
            p2s = coerce (Proxy :: Proxy String) p2
            n   = reduce op (_anum p1n) (_anum p2n)
            s   = reduce op (_astring p1s) (_astring p2s)
        in  (anum .~ n) bot `join` (astring .~ s) bot


-- | Path Sentivitity Framework
class Match a where
    -- | Match the tag-type field of an abstract primitive according to a
    -- given list of sub-type abstract values, and output a corresponding matched list
    match :: Coerce APrim tag => Proxy tag -> [a] -> Lens' APrim a -> APrim -> [Maybe APrim]

instance Match ABool where
    match proxy as lens p@APrim{..} = flip map as $ \field ->
        let tester = coerce proxy p `meet` ((lens .~ field) bot)
        in  if tester == bot
                then Nothing
                else (Just (APrim (_aundefined `meet'` field)
                                  (_anull `meet'` field)
                                  field
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
