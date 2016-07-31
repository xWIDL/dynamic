{-|
Module      : Core.Flow
Description : Data flow between labelled blocks
-}

module Core.Flow (Edge(..), Flow(..), Label) where

import Language.JS.Type
import Data.Set (Set)

-- | Edge connecting two blocks
data Edge a = Edge (a, a)
            | EnterTry (a, a) (a, Name)
            | ExitTry (a, a) a
            deriving (Eq, Ord, Show)

-- | AST that supports flow analysis with certain label
class Label a => Flow ast a where
    -- | Label of AST's entry point
    entryLabel :: ast a -> a
    -- | Label of AST's exit points
    exitLabels :: ast a -> Set a
    -- | Get flow graph of AST
    flow :: ast a -> Set (Edge a)

-- | Type that can be a label
class (Ord a, Show a) => Label a where
