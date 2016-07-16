{-|
Module      : Core.Abstract
Description : Typeclasses for value lattice-related abstraction
-}

module Core.Abstract (
  Lattice(..), Hom(..), Reduce(..), meet'
) where

-- | Types that support lattice-structure operations
class Eq a => Lattice a where
    -- | x U y
    join :: a -> a -> a
    -- | x ∩ y
    meet :: a -> a -> a
    -- | ⊤
    top  :: a
    -- | ⊥
    bot  :: a

    -- default implementation for a single-layer lattice
    join a b | a == top = top
             | b == top = top
             | a == bot = b
             | b == bot = a
             | a == b   = a
             | a /= b   = top

    meet a b | a == top = b
             | b == top = a
             | a == bot = bot
             | b == bot = bot
             | a == b   = a
             | a /= b   = bot

-- | Castable relationship
class Hom a b where
    -- | cast a to b
    hom :: a -> b

instance (Functor f, Hom a b) => Hom (f a) (f b) where
    hom = fmap hom

instance (Applicative f) => Hom a (f a) where
    hom = pure

-- | Indexed reduction relationship between the product of a type to the type,
-- representing a binary reduction
class Reduce a op where
    -- | reduce with the given operation
    reduce :: op -> a -> a -> a

-- | b ∩' a = b ∩' bs' = U_i (b ∩ b'_i)
meet' :: forall a b. Lattice b => Hom a [b] => b -> a -> b
meet' b a = let botb = bot :: b
            in  foldr (\b' ret -> (b `meet` b') `join` ret) botb (hom a :: [b])
