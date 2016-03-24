-- Abstract: Abstract the essentials out of value

module Core.Abstract where

class Eq a => Lattice a where
    unionAbs :: a -> a -> a
    topAbs   :: a

    unionAbs a b | a == b = a
                 | a /= b = topAbs

class Lattice a => Abstract c a where
    abstract   :: c -> a

class Lattice a => Computable a op where
    compute :: op -> a -> a -> a
