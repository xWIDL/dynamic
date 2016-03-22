module Example where

import AST
import Flow

newtype L = L { unL :: Int } deriving (Eq, Ord)

instance Show L where
    show (L i) = show i

instance Label L

example1 :: Program L
example1 = VarDecl (L 0) (Name "x")
