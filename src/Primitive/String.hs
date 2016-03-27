-- Abstract String
module Primitive.String where

import Core.Abstract
import AST

data AString = EmptyString | NonEmptyString | TopString | BotString deriving (Show, Eq)

instance Lattice AString where
    top = TopString
    bot = BotString

instance Hom String AString where
    hom "" = EmptyString
    hom _  = NonEmptyString

instance Hom AString String where
    hom EmptyString    = ""
    hom NonEmptyString = "s"

instance Reduce AString InfixOp where
    reduce OPlus EmptyString EmptyString = EmptyString
    reduce OPlus _           _           = NonEmptyString
    reduce _     _           _           = error "undefined operation over string"

instance Hom AString Bool where
    hom EmptyString     = False
    hom NonEmptyString  = True
