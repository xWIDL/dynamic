-- Abstract: Abstract the essentials out of value
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             AllowAmbiguousTypes, UndecidableInstances,
             ScopedTypeVariables #-}
module Core.Abstract where

class Eq a => Lattice a where
    join :: a -> a -> a
    meet :: a -> a -> a
    top  :: a
    bot  :: a

    -- default implementation for a flat lattice
    join a b | a == b = a
             | a /= b = top
    meet a b | a == b = a
             | a /= b = bot

class Hom a b where
    hom :: a -> b

class Reduce a op where
    reduce :: op -> a -> a -> a
