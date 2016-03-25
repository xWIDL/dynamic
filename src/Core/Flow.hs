-- Flow: Data Flow among labelled blocks
{-# LANGUAGE RankNTypes #-}
module Core.Flow where

import Data.Set (Set)
import Common

data Edge a = Edge (a, a)
            | EnterTry (a, a) (a, Name)
            | ExitTry (a, a) a
            deriving (Eq, Ord, Show)
            -- XXX: Maybe we should make inter-procedural transition an edge

class Label a => Flow ast a where
    initLabel :: ast a -> a
    finalLabels :: ast a -> Set a
    flow :: ast a -> Set (Edge a)

class (Ord a, Show a) => Label a where
