-- AST: A subset of JavaScript

{-# LANGUAGE TupleSections, DeriveTraversable, DeriveFunctor, DeriveFoldable,
             LambdaCase #-}

module AST where

import Core.Flow
import Data.Set hiding (foldr, map)
import qualified Data.Set as S
import qualified Data.Map as M

type Program a = Stmt a

newtype Name = Name String deriving (Eq, Ord)

instance Show Name where
    show (Name x) = x

data Stmt a = VarDecl a Name (Maybe (Expr a))
            | Assign a (LVal a) (Expr a)
            | If a (Expr a) (Stmt a) (Stmt a)
            | While a (Expr a) (Stmt a)
            | BreakStmt a
            | ContStmt a
            | Skip a
            | ReturnStmt a (Maybe (Expr a))
            | Seq (Stmt a) (Stmt a)
            | TryStmt a (Stmt a) a (Maybe (a, Name, Stmt a))
            | ThrowStmt a (Expr a)
            deriving (Eq, Functor, Foldable, Traversable)

data Expr a = PrimLit Prim
            | ObjExpr [(Name, Expr a)]
            | VarExpr Name
            | GetExpr (Expr a) Name
            | InfixExpr (Expr a) InfixOp (Expr a)
            | CallExpr (Expr a) [Expr a]
            | Closure a [Name] (Stmt a)
            deriving (Eq, Functor, Foldable, Traversable)

data Prim = PrimNum Double | PrimBool Bool | PrimStr String | PrimNull | PrimUndefined deriving (Eq)

data InfixOp = OPlus | OSubs | OMult | ODiv deriving (Eq)

data LVal a = LVar Name | LProp (Expr a) Name deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (LVal a) where
    show (LVar x) = show x
    show (LProp x a) = show x ++ "." ++ show a

instance Show a => Show (Stmt a) where
    show (VarDecl a x mExpr) = "var " ++ show x ++ " " ++ show a ++ mRHS
        where
            mRHS = case mExpr of
                Nothing -> ";"
                Just e  -> " = " ++ show e ++ ";"
    show (Assign a x expr) = show x ++ " " ++ show a ++ " = " ++ show expr ++ ";"
    show (If a e s1 s2) = "if (" ++ show e ++ ") " ++ show a ++
                          " {\n" ++ indent (show s1) ++ "} else {\n" ++ indent (show s2) ++ "}"
    show (While a e s) = "while (" ++ show e ++ ") " ++ show a ++ "{\n" ++ indent (show s) ++ "}"
    show (BreakStmt a) = "break " ++ show a ++ ";"
    show (ContStmt a) = "continue " ++ show a ++ ";"
    show (Skip a) = "skip " ++ show a ++ ";"
    show (ReturnStmt a mExpr) = case mExpr of
        Nothing -> "return " ++ show a ++ ";"
        Just e  -> "return " ++ show a ++ " " ++ show e ++ ";"
    show (Seq s1 s2) = show s1 ++ "\n" ++ show s2
    show (TryStmt l s exit Nothing) = "try " ++ show l ++ " {\n" ++ indent (show s) ++ "} " ++ show exit
    show (TryStmt l s exit (Just (lc, e, sc))) =
        "try " ++ show l ++ " {\n" ++ indent (show s) ++ "} " ++ show exit ++ " catch " ++
        show lc ++ " (" ++ show e ++ ") {\n" ++ indent (show sc) ++ "}"
    show (ThrowStmt l e) = "throw " ++ show l ++ " " ++ show e ++ ";"


instance Show a => Show (Expr a) where
    show (PrimLit prim) = show prim
    show (ObjExpr dict) = "{ " ++ sepByComma (map showEntry dict)++ " }"
        where
            showEntry (name, expr) = show name ++ " : " ++ show expr
    show (VarExpr x) = show x
    show (GetExpr e x) = "(" ++ show e ++ ")." ++ show x
    show (InfixExpr e1 op e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (CallExpr e args) = "(" ++ show e ++ ")(" ++ sepByComma (map show args) ++ ")"
    show (Closure a args body) = "function " ++ show a ++ " (" ++ sepByComma (map show args) ++ ") {\n" ++ indent (show body) ++ "}"

instance Show Prim where
    show (PrimNum d)  = show d
    show (PrimBool b) = if b then "true" else "false"
    show (PrimStr s)  = show s
    show PrimNull     = "null"
    show PrimUndefined = "undefined"

instance Show InfixOp where
    show OPlus = "+"
    show OSubs = "-"
    show ODiv  = "/"
    show OMult = "*"

sepByComma :: [String] -> String
sepByComma [] = ""
sepByComma [x] = x
sepByComma (x:xs) = x ++ ", " ++ sepByComma xs

indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- Flow implementation

instance Label a => Flow Stmt a where
    initLabel (VarDecl l _ _)       = l
    initLabel (Assign l _ _)        = l
    initLabel (If l _ _ _)          = l
    initLabel (While l _ _)         = l
    initLabel (BreakStmt l)         = l
    initLabel (ContStmt l)          = l
    initLabel (Skip l)              = l
    initLabel (ReturnStmt l _)      = l
    initLabel (Seq s _)             = initLabel s
    initLabel (TryStmt l _ _ _)     = l
    initLabel (ThrowStmt l _)       = l

    finalLabels (VarDecl l _ _)     = singleton l
    finalLabels (Assign l _ _)      = singleton l
    finalLabels (If _ _ s1 s2)      = finalLabels s1 `union` finalLabels s2
    finalLabels (While l _ s)       = singleton l `union` S.fromList (scanBreak s id)
    finalLabels (BreakStmt l)       = singleton l
    finalLabels (ContStmt l)        = singleton l
    finalLabels (Skip l)            = singleton l
    finalLabels (ReturnStmt l _)    = singleton l
    finalLabels (Seq s1 s2)         = finalLabels s2
    finalLabels (TryStmt _ s _ Nothing) = finalLabels s
    finalLabels (TryStmt _ _ exit (Just (_, _, sc))) = singleton exit `union` finalLabels sc
    finalLabels (ThrowStmt l _)     = singleton l

    flow (If l _ s1 s2) = fromList [Edge (l, initLabel s1), Edge (l, initLabel s2)] `union`
                          flow s1 `union` flow s2
    flow (While l _ s)  = singleton (Edge (l, initLabel s)) `union`
                          S.fromList (scanCont s (\l' -> Edge (l', l))) `union`
                          flow s
    flow (Seq s1 s2)    = flow s1 `union` S.map (\l -> Edge (l, initLabel s2)) (finalLabels s1) `union` flow s2
    flow (TryStmt l s _ Nothing) = flow s `union` S.singleton (Edge (l, initLabel s))
    flow (TryStmt l s exit (Just (lc, e, sc))) =
        flow s `union` S.singleton (Edge (l, initLabel s)) `union`
        flow sc `union` S.singleton (Edge (lc, initLabel sc)) `union`
        S.map (\l' -> ExitTry (l', exit) l) (finalLabels s) `union`
        S.singleton (EnterTry (l, initLabel s) lc)
    flow _              = empty

scanCont :: Stmt a -> (a -> b) -> [b]
scanCont (ContStmt l) f = [f l]
scanCont (Seq s1 s2) f = scanCont s1 f ++ scanCont s2 f
scanCont (If _ _ s1 s2) f = scanCont s1 f ++ scanCont s2 f
scanCont _ _ = []

scanBreak :: Stmt a -> (a -> b) -> [b]
scanBreak (BreakStmt l) f = [f l]
scanBreak (Seq s1 s2) f = scanBreak s1 f ++ scanBreak s2 f
scanBreak (If _ _ s1 s2) f = scanBreak s1 f ++ scanBreak s2 f
scanBreak _ _ = []

labelsOf :: Label a => Stmt a -> M.Map a (Stmt a)
labelsOf s = case s of
    VarDecl l _ _   -> M.singleton l s
    Assign l _ _    -> M.singleton l s
    If l _ s1 s2    -> M.singleton l s `M.union` labelsOf s1 `M.union` labelsOf s2
    While l _ s     -> M.singleton l s `M.union` labelsOf s
    BreakStmt l     -> M.singleton l s
    ContStmt l      -> M.singleton l s
    Skip l          -> M.singleton l s
    ReturnStmt l _  -> M.singleton l s
    Seq s1 s2       -> labelsOf s1 `M.union` labelsOf s2
    TryStmt l s' _ Nothing
                    -> M.singleton l s `M.union` labelsOf s'
    TryStmt l s' _ (Just (lc, _, sc))
                    -> M.singleton l s `M.union` labelsOf s' `M.union` labelsOf sc
    ThrowStmt l _   -> M.singleton l s
