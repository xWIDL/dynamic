-- Abstract Primitives

module APrim where

import Core.Abstract
import AST

data APrim = APrimNum ANum | APrimBool ABool | APrimStr AString | APrimNull | APrimUndefined deriving (Show, Eq)

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
    compute _ _ _ = TopNum -- FIXME: boilerplate code

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
    compute _ _ _ = TopString -- FIXME: boilerplate code

instance Lattice APrim where
    topAbs = APrimUndefined

instance Abstract Prim APrim where
    abstract (PrimNum n)   = APrimNum  $ abstract n
    abstract (PrimStr n)   = APrimStr  $ abstract n
    abstract (PrimBool n)  = APrimBool $ abstract n
    abstract PrimNull      = APrimNull
    abstract PrimUndefined = APrimUndefined

instance Computable APrim InfixOp where
    compute _ _ _ = APrimUndefined -- FIXME: boilerplate code
