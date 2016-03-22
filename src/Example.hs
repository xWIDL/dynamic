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
example1 = VarDecl (L 0) (Name "x") (Just (PrimLit (PrimInt 0)))

{-
    var f = function(a) {
        return function (b) {
            return a + b;
        };
    };

    var f1 = f(1);
    var f2 = f(2);
    var x  = f1(2); // should be 3

-}
example2 :: Program L
example2 = VarDecl (L 0) (Name "f") (Just (Closure (L 10) [Name "a"]
                (ReturnStmt (L 2) (Just (Closure (L 11) [Name "b"]
                    (ReturnStmt (L 3) (Just (InfixExpr (VarExpr (Name "a")) OPlus (VarExpr (Name "b")))))))))) `Seq`
           VarDecl (L 4) (Name "f1") (Just (CallExpr (VarExpr (Name "f")) [PrimLit (PrimInt 1)])) `Seq`
           VarDecl (L 5) (Name "f2") (Just (CallExpr (VarExpr (Name "f")) [PrimLit (PrimInt 2)])) `Seq`
           VarDecl (L 8) (Name "x") (Just (CallExpr (VarExpr (Name "f1")) [PrimLit (PrimInt 2)]))


{-
    var f = function (a) {
        var x = 1;
        return x + a;
    };

    var x = f(2); // Should be 3
-}

example3 :: Program L
example3 = VarDecl (L 0) (Name "f") (Just (Closure (L 10) [Name "a"]
                (VarDecl (L 2) (Name "x") (Just (PrimLit (PrimInt 1))) `Seq`
                 ReturnStmt (L 4) (Just (InfixExpr (VarExpr (Name "x")) OPlus (VarExpr (Name "a"))))))) `Seq`
           VarDecl (L 2) (Name "x") (Just (CallExpr (VarExpr (Name "f")) [PrimLit (PrimInt 2)]))
