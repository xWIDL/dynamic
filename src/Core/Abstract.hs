-- Abstract: Abstract the essentials out of value
module Core.Abstract (
  Lattice(..), Hom(..), Reduce(..), meet'
) where

class Eq a => Lattice a where
    join :: a -> a -> a
    meet :: a -> a -> a
    top  :: a
    bot  :: a

    -- default implementation
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

-- Homomorphism
class Hom a b where
    hom :: a -> b

instance (Functor f, Hom a b) => Hom (f a) (f b) where
    hom = fmap hom

instance (Applicative f) => Hom a (f a) where
    hom = pure

-- Monomorphic reduction
class Reduce a op where
    reduce :: op -> a -> a -> a

-- Augmented meet
meet' :: forall a b. Lattice b => Hom a [b] => b -> a -> b
meet' b a = let botb = bot :: b
            in  foldr (\b' ret -> (b `meet` b') `join` ret) botb (hom a :: [b])
