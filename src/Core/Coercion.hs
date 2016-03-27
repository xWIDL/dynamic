-- Coercion Framework
{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}

module Core.Coercion where

import Core.Abstract
import Common

class (Lattice a, Lattice b, Hom a c, Hom c b) => HomLattice a (c :: *) b where
    -- Mapping from lattice a to lattice b indexed by concrete type c
    homlat :: Proxy c -> a -> b

    homlat _ a | a == top  = top
               | a == bot  = bot
               | otherwise = hom (hom a :: c) :: b

class Coerce super aconc where
    coerce :: Proxy aconc -> super -> super
