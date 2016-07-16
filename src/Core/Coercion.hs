{-|
Module      : Core.Coercion
Description : Typeclasses for abstracting coercion between lattices
-}

{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}

module Core.Coercion (HomLattice(..), Coerce(..)) where

import Core.Abstract
import Common

-- | Castable relationship between two lattices
class (Lattice a, Lattice b, Hom a c, Hom c b) => HomLattice a (c :: *) b where
    -- | Mapping from lattice a to lattice b indexed by concrete type c
    homlat :: Proxy c -> a -> b

    homlat _ a | a == top  = top
               | a == bot  = bot
               | otherwise = hom (hom a :: c) :: b

-- | Typed-indexed castable relationship within a type,
-- used by APrim to project to a specific sub-lattice
class Coerce a tag where
    -- | coarce given type tag
    coerce :: Proxy tag -> a -> a
