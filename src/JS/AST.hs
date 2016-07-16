{-|
Module      : JS.AST
Description : A subset of JavaScript
-}

{-# LANGUAGE TupleSections, DeriveTraversable, DeriveFunctor, DeriveFoldable, LambdaCase #-}

module JS.AST (
  Program, Stmt(..), Expr(..), InfixOp(..), LVal(..),
  labelsOf
) where

import Core.Flow
import JS.Type

import Data.Set hiding (foldr, map)
import qualified Data.Set as S
import qualified Data.Map as M

-- | JS program
type Program a = Stmt a

-- | JS statement
data Stmt a = VarDecl a Name (Maybe (Expr a))   -- ^ > var <x> [= <expr>]
            | Assign a (LVal a) (Expr a)        -- ^ > <x> = <expr>
            | If a (Expr a) (Stmt a) (Stmt a)   -- ^ > if (<expr>) { <stmt> } else { <stmt> }
            | While a (Expr a) (Stmt a)         -- ^ > while (<expr>) { <stmt> }
            | BreakStmt a                       -- ^ > break
            | ContStmt a                        -- ^ > continue
            | Skip a                            -- ^ Do nothing
            | ReturnStmt a (Maybe (Expr a))     -- ^ > return [<expr>]
            | Seq (Stmt a) (Stmt a)             -- ^ > <stmt> ; <stmt>
            | TryStmt a (Stmt a) a (Maybe (a, Name, Stmt a)) -- ^ try  { <stmt> } [catch (<x>) { <stmt> } ]
            | ThrowStmt a (Expr a) -- ^ > throw <expr>
            | InvokeStmt a (Expr a) Name [Expr a] -- > ^ (<expr>).<f>(<expr>*)
            deriving (Eq, Functor, Foldable, Traversable)

-- | JS expression
data Expr a = PrimLit Prim -- ^ primitive literal
            | ObjExpr [(Name, Expr a)] -- ^ object expression
            | VarExpr Name -- ^ variable
            | GetExpr (Expr a) Name -- ^ > (<expr>).<x>
            | InfixExpr (Expr a) InfixOp (Expr a) -- ^ > <expr> <op> <expr>
            | CallExpr (Expr a) [Expr a] -- ^ @ (<expr>)(<expr>*) @, the first might be evaluated to a closure
            | Closure a [Name] (Stmt a) -- ^ > function (<x>*) { <stmt> }
            deriving (Eq, Functor, Foldable, Traversable)

-- | Infix binary operator
data InfixOp = OPlus | OSubs | OMult | ODiv deriving (Eq)

-- | Left-hand-side value
data LVal a = LVar Name -- ^ variable
            | LAttr (Expr a) Name -- ^ > (<expr>).<x>
            deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (LVal a) where
    show (LVar x) = show x
    show (LAttr x a) = show x ++ "." ++ show a

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
    show (InvokeStmt l e n args) = show l ++ "(" ++ show e ++ ")." ++ show n ++ "(" ++ sepByComma (map show args) ++ ")"

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
    entryLabel (VarDecl l _ _)       = l
    entryLabel (Assign l _ _)        = l
    entryLabel (If l _ _ _)          = l
    entryLabel (While l _ _)         = l
    entryLabel (BreakStmt l)         = l
    entryLabel (ContStmt l)          = l
    entryLabel (Skip l)              = l
    entryLabel (ReturnStmt l _)      = l
    entryLabel (Seq s _)             = entryLabel s
    entryLabel (TryStmt l _ _ _)     = l
    entryLabel (ThrowStmt l _)       = l
    entryLabel (InvokeStmt l _ _ _)  = l

    exitLabels (VarDecl l _ _)     = singleton l
    exitLabels (Assign l _ _)      = singleton l
    exitLabels (If _ _ s1 s2)      = exitLabels s1 `union` exitLabels s2
    exitLabels (While l _ s)       = singleton l `union` S.fromList (scanBreak s id)
    exitLabels (BreakStmt l)       = singleton l
    exitLabels (ContStmt l)        = singleton l
    exitLabels (Skip l)            = singleton l
    exitLabels (ReturnStmt l _)    = singleton l
    exitLabels (Seq _s1 s2)        = exitLabels s2
    exitLabels (TryStmt _ s _ Nothing) = exitLabels s
    exitLabels (TryStmt _ _ exit (Just (_, _, sc))) = singleton exit `union` exitLabels sc
    exitLabels (ThrowStmt l _)     = singleton l
    exitLabels (InvokeStmt l _ _ _) = singleton l

    flow (If l _ s1 s2) = fromList [Edge (l, entryLabel s1), Edge (l, entryLabel s2)] `union`
                          flow s1 `union` flow s2
    flow (While l _ s)  = singleton (Edge (l, entryLabel s)) `union`
                          S.fromList (scanCont s (\l' -> Edge (l', l))) `union`
                          flow s
    flow (Seq s1 s2)    = flow s1 `union` S.map (\l -> Edge (l, entryLabel s2)) (exitLabels s1) `union` flow s2
    flow (TryStmt l s _ Nothing) = flow s `union` S.singleton (Edge (l, entryLabel s))
    flow (TryStmt l s exit (Just (lc, e, sc))) =
        flow s `union` S.singleton (Edge (l, entryLabel s)) `union`
        flow sc `union` S.singleton (Edge (lc, entryLabel sc)) `union`
        S.map (\l' -> ExitTry (l', exit) l) (exitLabels s) `union`
        S.singleton (EnterTry (l, entryLabel s) (lc, e))
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

-- | Labelling of a statement
labelsOf :: Label a => Stmt a -> M.Map a (Stmt a)
labelsOf s = case s of
    VarDecl l _ _   -> M.singleton l s
    Assign l _ _    -> M.singleton l s
    If l _ s1 s2    -> M.singleton l s `M.union` labelsOf s1 `M.union` labelsOf s2
    While l _ s'    -> M.singleton l s' `M.union` labelsOf s
    BreakStmt l     -> M.singleton l s
    ContStmt l      -> M.singleton l s
    Skip l          -> M.singleton l s
    ReturnStmt l _  -> M.singleton l s
    Seq s1 s2       -> labelsOf s1 `M.union` labelsOf s2
    TryStmt l s' _ Nothing
                    -> M.singleton l s `M.union` labelsOf s'
    TryStmt l s' _ (Just (_lc, _, sc))
                    -> M.singleton l s `M.union` labelsOf s' `M.union` labelsOf sc
    ThrowStmt l _   -> M.singleton l s
    InvokeStmt l _ _ _ -> M.singleton l s
