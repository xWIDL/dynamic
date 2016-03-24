-- Abstract Primitives

module APrim where

import Core.Abstract
import AST

data APrim = APrimNum ANum | APrimBool ABool | APrimStr AString | APrimNull | APrimUndefined deriving (Show, Eq)
-- FIXME: In JavaScript, 1 / 0 will give Infinity

data ANum = NegNum | ZeroNum | PosNum | TopNum deriving (Show, Eq)
data ABool = FalseBool | TrueBool | TopBool deriving (Show, Eq)
data AString = EmptyString | NonEmptyString | TopString deriving (Show, Eq)

instance Lattice ANum where
    topAbs = TopNum

instance Abstract Double ANum where
    abstract x | x >  0 = PosNum
               | x <  0 = NegNum
               | x == 0 = ZeroNum

instance Computable ANum InfixOp where
    compute OPlus   PosNum    PosNum  = PosNum
    compute OPlus   PosNum    NegNum  = TopNum
    compute OPlus   NegNum    NegNum  = NegNum
    compute OPlus   NegNum    PosNum  = TopNum
    compute OPlus   x         ZeroNum = x
    compute OPlus   ZeroNum   x       = x

    compute OSubs   PosNum    NegNum  = PosNum
    compute OSubs   PosNum    PosNum  = TopNum
    compute OSubs   NegNum    PosNum  = NegNum
    compute OSubs   NegNum    NegNum  = TopNum
    compute OSubs   x         ZeroNum = x
    compute OSubs   ZeroNum   x       = neg x

    compute OMult   _         ZeroNum = ZeroNum
    compute OMult   x         PosNum  = x
    compute OMult   x         NegNum  = neg x

    compute ODiv    _         ZeroNum = error "Can't process division by zero"
    compute ODiv    x         PosNum  = x
    compute ODiv    x         NegNum  = neg x

neg :: ANum -> ANum
neg ZeroNum = ZeroNum
neg NegNum  = PosNum
neg PosNum  = NegNum

instance Lattice ABool where
    topAbs = TopBool

instance Abstract Bool ABool where
    abstract True = TrueBool
    abstract False = FalseBool

instance Computable ABool InfixOp where
    compute _ _ _ = TopBool -- FIXME: boilerplate code

instance Lattice AString where
    topAbs = TopString

instance Abstract String AString where
    abstract "" = EmptyString
    abstract _  = NonEmptyString

instance Computable AString InfixOp where
    compute OPlus EmptyString EmptyString = EmptyString
    compute OPlus _           _           = NonEmptyString
    compute _     _           _           = error "undefined operation over string"

instance Lattice APrim where
    topAbs = APrimUndefined

instance Abstract Prim APrim where
    abstract (PrimNum n)   = APrimNum  $ abstract n
    abstract (PrimStr n)   = APrimStr  $ abstract n
    abstract (PrimBool n)  = APrimBool $ abstract n
    abstract PrimNull      = APrimNull
    abstract PrimUndefined = APrimUndefined

-- Coersion or a more general and compostional framework
instance Computable APrim InfixOp where
    compute op (APrimNum n1)  (APrimNum n2)  = APrimNum  $ compute op n1 n2
    compute op (APrimBool b1) (APrimBool b2) = APrimBool $ compute op b1 b2
    compute op (APrimStr s1)  (APrimStr s2)  = APrimStr  $ compute op s1 s2
    compute op _             _               = APrimUndefined
