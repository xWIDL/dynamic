module Example where

import AST
import Flow

newtype L = L { unL :: Int } deriving (Eq, Ord)

instance Show L where
    show (L i) = "@" ++ show i

instance Label L

{-
    var x = 0;
-}
example1 :: Program L
example1 = VarDecl (L 0) (Name "x")

{-
    var f = function(a) {
        return function (b) {
            return a + b;
        };
    };

    var f1 = f(1);
    var f2 = f(2);
    var x  = f1(2);

-}
example2 :: Program L
example2 = VarDecl (L 0) (Name "f") `Seq`
           Assign (L 1) (Name "f") (Closure (L 10) [Name "a"]
                (ReturnStmt (L 2) (Just (Closure (L 11) [Name "b"]
                    (ReturnStmt (L 3) (Just (InfixExpr (VarExpr (Name "a")) OPlus (VarExpr (Name "b"))))))))) `Seq`
           VarDecl (L 4) (Name "f1") `Seq`
           VarDecl (L 5) (Name "f2") `Seq`
           Assign (L 6) (Name "f1") (CallExpr (VarExpr (Name "f")) [PrimLit (PrimInt 1)]) `Seq`
           Assign (L 7) (Name "f2") (CallExpr (VarExpr (Name "f")) [PrimLit (PrimInt 2)]) `Seq`
           VarDecl (L 8) (Name "x") `Seq`
           Assign (L 9) (Name "x") (CallExpr (VarExpr (Name "f1")) [PrimLit (PrimInt 2)])
