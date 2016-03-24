-- Abstract Primitive

module APrim where

import Core.Abstract
import AST

data APrim = APrimNum ANum | APrimBool ABool | APrimStr AString
           | APrimNull | APrimUndefined | APrimBot deriving (Show, Eq)
data ANum = NegNum | ZeroNum | PosNum | TopNum | BotNum deriving (Show, Eq)
data ABool = FalseBool | TrueBool | TopBool | BotBool deriving (Show, Eq)
data AString = EmptyString | NonEmptyString | TopString | BotString deriving (Show, Eq)

-- FIXME: In JavaScript, 1 / 0 will give Infinity

-- Abstract Number

instance Lattice ANum where
    top = TopNum
    bot = BotNum

instance Hom Double ANum where
    hom x | x >  0 = PosNum
          | x <  0 = NegNum
          | x == 0 = ZeroNum

instance Reduce ANum InfixOp where
    reduce OPlus   PosNum    PosNum  = PosNum
    reduce OPlus   PosNum    NegNum  = TopNum
    reduce OPlus   NegNum    NegNum  = NegNum
    reduce OPlus   NegNum    PosNum  = TopNum
    reduce OPlus   x         ZeroNum = x
    reduce OPlus   ZeroNum   x       = x

    reduce OSubs   PosNum    NegNum  = PosNum
    reduce OSubs   PosNum    PosNum  = TopNum
    reduce OSubs   NegNum    PosNum  = NegNum
    reduce OSubs   NegNum    NegNum  = TopNum
    reduce OSubs   x         ZeroNum = x
    reduce OSubs   ZeroNum   x       = neg x

    reduce OMult   _         ZeroNum = ZeroNum
    reduce OMult   x         PosNum  = x
    reduce OMult   x         NegNum  = neg x

    reduce ODiv    _         ZeroNum = error "Can't process division by zero"
    reduce ODiv    x         PosNum  = x
    reduce ODiv    x         NegNum  = neg x

neg :: ANum -> ANum
neg ZeroNum = ZeroNum
neg NegNum  = PosNum
neg PosNum  = NegNum

-- Abstract Bool

instance Lattice ABool where
    top = TopBool
    bot = BotBool

instance Hom Bool ABool where
    hom True = TrueBool
    hom False = FalseBool

instance Reduce ABool InfixOp where
    reduce _ _ _ = TopBool -- FIXME: boilerplate code

-- Abstract String

instance Lattice AString where
    top = TopString
    bot = BotString

instance Hom String AString where
    hom "" = EmptyString
    hom _  = NonEmptyString

instance Reduce AString InfixOp where
    reduce OPlus EmptyString EmptyString = EmptyString
    reduce OPlus _           _           = NonEmptyString
    reduce _     _           _           = error "undefined operation over string"

-- Coercion Framework

instance Hom ANum AString where
    hom BotNum = BotString
    hom _      = NonEmptyString

instance Hom ABool AString where
    hom BotBool = BotString
    hom _       = NonEmptyString

instance Hom ANum ABool where
    hom NegNum  = TrueBool
    hom PosNum  = TrueBool
    hom ZeroNum = FalseBool
    hom TopNum  = TopBool
    hom BotNum  = BotBool

instance Hom ABool ANum where
    hom TrueBool  = PosNum
    hom FalseBool = ZeroNum
    hom BotBool   = BotNum
    hom TopBool   = TopNum

-- Abstract Primitive

instance Lattice APrim where
    top = APrimUndefined
    bot = APrimBot

instance Hom Prim APrim where
    hom (PrimNum n)   = APrimNum  $ hom n
    hom (PrimStr n)   = APrimStr  $ hom n
    hom (PrimBool n)  = APrimBool $ hom n
    hom PrimNull      = APrimNull
    hom PrimUndefined = APrimUndefined

-- FIXME: Coercion or a more general and compostional framework
instance Reduce APrim InfixOp where
    -- Homogenous Reduction
    reduce op (APrimNum n1)  (APrimNum n2)  = APrimNum  $ reduce op n1 n2
    reduce op (APrimBool b1) (APrimBool b2) = APrimBool $ reduce op b1 b2
    reduce op (APrimStr s1)  (APrimStr s2)  = APrimStr  $ reduce op s1 s2

    -- Heterogenous Reduction
    -- NOTE: The priority of coercion is hard-encoded
    reduce op (APrimNum n) (APrimBool b) = APrimNum $ reduce op n (hom b)
    reduce op (APrimBool b) (APrimNum n) = APrimNum $ reduce op (hom b) n

    reduce op (APrimStr s) (APrimNum n)  = APrimStr $ reduce op s (hom n)
    reduce op (APrimNum n) (APrimStr s)  = APrimStr $ reduce op (hom n) s
    reduce op (APrimStr s) (APrimBool b) = APrimStr $ reduce op s (hom b)
    reduce op (APrimBool b) (APrimStr s) = APrimStr $ reduce op (hom b) s

    -- undefined
    reduce op _ _ = APrimUndefined
