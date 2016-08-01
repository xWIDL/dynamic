{-|
Module      : JS.AST
Description : A subset of JavaScript
-}

{-# LANGUAGE TupleSections, DeriveTraversable, DeriveFunctor, DeriveFoldable, LambdaCase #-}

module JS.AST (
  Program, Stmt(..), Expr(..), InfixOp(..), LVal(..),
  labelsOf, prettyPrint
) where

import Core.Flow
import Language.JS.Type

import Text.PrettyPrint.Leijen

import Data.Set hiding (foldr, empty, map)
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
            deriving (Show, Eq, Functor, Foldable, Traversable)

-- | JS expression
data Expr a = PrimLit Prim -- ^ primitive literal
            | ObjExpr [(Name, Expr a)] -- ^ object expression
            | VarExpr Name -- ^ variable
            | GetExpr (Expr a) Name -- ^ > (<expr>).<x>
            | InfixExpr (Expr a) InfixOp (Expr a) -- ^ > <expr> <op> <expr>
            | CallExpr (Expr a) [Expr a] -- ^ @ (<expr>)(<expr>*) @, the first might be evaluated to a closure
            | Closure a [Name] (Stmt a) -- ^ > function (<x>*) { <stmt> }
            deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Infix binary operator
data InfixOp = OPlus | OSubs | OMult | ODiv deriving (Show, Eq)

-- | Left-hand-side value
data LVal a = LVar Name -- ^ variable
            | LAttr (Expr a) Name -- ^ > (<expr>).<x>
            deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty Name where
    pretty = text . show

instance Show a => Pretty (LVal a) where
    pretty (LVar x) = pretty x
    pretty (LAttr x a) = pretty x <> dot <> pretty a

instance Show a => Pretty (Stmt a) where
    pretty (VarDecl a x mExpr) = text "var" <+> pretty x <+> text (show a) <> mRHS
        where
            mRHS = case mExpr of
                Nothing -> semi
                Just e  -> space <> equals <+> pretty e <> semi

    pretty (Assign a x expr) = pretty x <+> text (show a) <+> equals <+> pretty expr <> semi

    pretty (If a e s1 s2) = text "if (" <> pretty e <> text ")" <+> text (show a) <+>
                            braces (line <> indent 4 (pretty s1)) <+> text "else" <+>
                            braces (line <> indent 4 (pretty s2))

    pretty (While a e s) = text "while" <+> parens (pretty e) <+> text (show a) <>
                           braces (line <> indent 4 (pretty s))

    pretty (BreakStmt a) = text "break" <+> text (show a) <> semi

    pretty (ContStmt a) = text "continue" <+> text (show a) <> semi

    pretty (Skip a) = text "skip" <+> text (show a) <> semi

    pretty (ReturnStmt a mExpr) = case mExpr of
        Nothing -> text "return" <+> text (show a) <> semi
        Just e  -> text "return" <+> text (show a) <+> pretty e <> semi

    pretty (Seq s1 s2) = pretty s1 <> line <> pretty s2

    pretty (TryStmt l s exit Nothing) =
        text "try" <+> text (show l) <+>
        braces (line <> indent 4 (pretty s)) <+> text (show exit)

    pretty (TryStmt l s exit (Just (lc, e, sc))) =
        text "try" <+> text (show l) <+> braces (line <> indent 4 (pretty s)) <+>
        text (show exit) <> text "catch" <+>
        text (show lc) <+> parens (pretty e) <+> braces (line <> indent 4 (pretty sc))

    pretty (ThrowStmt l e) = text "throw" <+> text (show l) <+> pretty e <> semi

    pretty (InvokeStmt l e n args) = text (show l) <> parens (pretty e) <> dot <> pretty n <>
                                     encloseSep lparen rparen (comma <> space) (map pretty args)

instance Show a => Pretty (Expr a) where
    pretty (PrimLit prim) = pretty prim
    pretty (ObjExpr dict) = encloseSep lbrace rbrace (comma <> space) (map prettyEntry dict)
        where
            prettyEntry (name, expr) = pretty name <+> colon <+> pretty expr
    pretty (VarExpr x) = pretty x
    pretty (GetExpr e x) = parens (pretty e) <> dot <> pretty x
    pretty (InfixExpr e1 op e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
    pretty (CallExpr e args) = parens (pretty e) <> encloseSep lparen rparen (comma <> space) (map pretty args)
    pretty (Closure a args body) = text "function" <+> text (show a) <+> 
                                   encloseSep lparen rparen (comma <> space) (map pretty args) <+>
                                   braces (line <> indent 4 (pretty body))

instance Pretty InfixOp where
    pretty OPlus = text "+"
    pretty OSubs = text "-"
    pretty ODiv  = text "/"
    pretty OMult = text "*"

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
    flow _              = S.empty

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


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty
