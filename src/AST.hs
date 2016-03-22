{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, TupleSections #-}

module AST where

import Flow
import Data.Set hiding (foldr, map)
import qualified Data.Set as S
import qualified Data.Map as M

type Program a = Stmt a

newtype Name = Name String deriving (Show, Eq, Ord)

data Stmt a = VarDecl a Name
            | Assign a Name (Expr a)
            | If a (Expr a) (Stmt a) (Stmt a)
            | While a (Expr a) (Stmt a)
            | BreakStmt a
            | ContStmt a
            | Skip a
            | ReturnStmt a (Maybe (Expr a))
            | Seq (Stmt a) (Stmt a)
            deriving (Show, Eq)

data Expr a = PrimLit Prim
            | ObjExpr [(Name, Expr a)]
            | VarExpr Name
            | GetExpr (Expr a) Name
            | InfixExpr (Expr a) InfixOp (Expr a)
            | CallExpr (Expr a) [Expr a]
            | Closure [Name] (Stmt a)
            deriving (Show, Eq)

data Prim = PrimInt Int | PrimBool Bool | PrimNull | PrimUndefined deriving (Show, Eq)

data InfixOp = OPlus | OSubs | OMult | ODiv deriving (Show, Eq)

-- Flow implementation

instance Label a => Flow Stmt a where
    initLabel (VarDecl l _)         = l
    initLabel (Assign l _ _)        = l
    initLabel (If l _ _ _)          = l
    initLabel (While l _ _)         = l
    initLabel (BreakStmt l)         = l
    initLabel (ContStmt l)          = l
    initLabel (Skip l)              = l
    initLabel (ReturnStmt l _)      = l
    initLabel (Seq s _)             = initLabel s

    finalLabels (VarDecl l _)       = singleton l
    finalLabels (Assign l _ _)      = singleton l
    finalLabels (If _ _ s1 s2)      = finalLabels s1 `union` finalLabels s2
    finalLabels (While l _ _)       = singleton l
    finalLabels (BreakStmt l)       = singleton l
    finalLabels (ContStmt l)        = singleton l
    finalLabels (Skip l)            = singleton l
    finalLabels (ReturnStmt l _)    = singleton l
    finalLabels (Seq s1 s2)         = finalLabels s2

    flow (If l _ s1 s2) = fromList [(l, initLabel s1), (l, initLabel s2)] `union`
                          flow s1 `union` flow s2
    flow (While l _ s)  = singleton (l, initLabel s) `union` (S.map (,l) (finalLabels s))
    flow (Seq s1 s2)    = flow s1 `union` S.map (,initLabel s2) (finalLabels s1) `union` flow s2
    flow _              = empty

labelsOf :: Label a => Stmt a -> M.Map a (Stmt a)
labelsOf s = case s of
    VarDecl l _     -> M.singleton l s
    Assign l _ _    -> M.singleton l s
    If l _ s1 s2    -> M.singleton l s `M.union` labelsOf s1 `M.union` labelsOf s2
    While l _ s     -> M.singleton l s `M.union` labelsOf s
    BreakStmt l     -> M.singleton l s
    ContStmt l      -> M.singleton l s
    Skip l          -> M.singleton l s
    ReturnStmt l _  -> M.singleton l s
    Seq s1 s2       -> labelsOf s1 `M.union` labelsOf s2
