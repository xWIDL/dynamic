-- Abstract Number
module Primitive.Number where

import Core.Abstract
import AST

data ANum = NegNum | ZeroNum | PosNum | TopNum | BotNum deriving (Show, Eq)

instance Lattice ANum where
    top = TopNum
    bot = BotNum

instance Hom Double ANum where
    hom x | x >  0 = PosNum
          | x <  0 = NegNum
          | x == 0 = ZeroNum

instance Hom ANum Double where
    hom ZeroNum = 0.0
    hom NegNum  = -1.0
    hom PosNum  = 1.0

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

instance Hom ANum String where
    hom ZeroNum = show (hom ZeroNum :: Double)
    hom PosNum  = show (hom ZeroNum :: Double)
    hom NegNum  = show (hom PosNum  :: Double)

instance Hom ANum Bool where
    hom NegNum  = (hom NegNum  :: Double) /= 0.0
    hom PosNum  = (hom PosNum  :: Double) /= 0.0
    hom ZeroNum = (hom ZeroNum :: Double) /= 0.0
