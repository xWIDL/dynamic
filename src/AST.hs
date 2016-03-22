module AST where

newtype Name = Name String deriving (Show, Eq)

data Stmt a = VarDecl a Name
            | Assign a Name Expr
            | If a Expr (Stmt a) (Stmt a)
            | While a Expr (Stmt a)
            | BreakStmt a
            | ContStmt a
            | Skip a
            | ReturnStmt a (Maybe Expr)
            | Seq (Stmt a) (Stmt a)
            deriving (Show, Eq)

data Expr = PrimLit Prim
          | ObjExpr [(Name, Expr)]
          | VarExpr Name
          | GetExpr Expr Name
          | InfixExpr Expr InfixOp Expr
          | CallExpr Expr [Expr]
          deriving (Show, Eq)

data Prim = PrimInt Int | PrimBool Bool | PrimNull deriving (Show, Eq)

data InfixOp = OPlus | OSubs | OMult | ODiv deriving (Show, Eq)
