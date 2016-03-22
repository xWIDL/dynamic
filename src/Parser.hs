{-# LANGUAGE LambdaCase #-}
module Parser where

import Language.ECMAScript3.Syntax.Annotations
import qualified Language.ECMAScript3 as ES

import Flow
import AST

newtype L = L { unL :: Int } deriving (Eq, Ord)

instance Show L where
    show (L i) = "@" ++ show i

instance Label L

parseJS :: String -> Either String (Program L)
parseJS s = case ES.parseFromString s of
    Right js -> translate js >>= return . relabel
    Left err -> Left $ show err

relabel :: Program a -> Program L
relabel prog = reannotate (L . snd) . fst $ assignUniqueIds 0 prog

translate :: Show a => ES.JavaScript a -> Either String (Program a)
translate = translateStmts . ES.unJavaScript

translateStmts :: Show a => [ES.Statement a] -> Either String (Stmt a)
translateStmts stmts = foldr1 Seq <$> mapM translateStmt stmts

translateStmt :: Show a => ES.Statement a -> Either String (Stmt a)
translateStmt (ES.ExprStmt a e) = translateExprStmt a e

translateStmt (ES.IfStmt a e1 s1 s2) = do
    e <- translateExpr e1
    s1' <- translateStmt s1
    s2' <- translateStmt s2
    return (If a e s1' s2')

translateStmt (ES.WhileStmt a e s) = do
    e' <- translateExpr e
    s' <- translateStmt s
    return (While a e' s')

translateStmt (ES.ReturnStmt a me) =
    case me of
        Nothing -> return (ReturnStmt a Nothing)
        Just e  -> do
            e' <- translateExpr e
            return (ReturnStmt a (Just e'))

translateStmt (ES.VarDeclStmt a decls) = foldr1 Seq <$> mapM translateVarDecl decls

translateStmt (ES.FunctionStmt a f args ss) = do
    s' <- translateStmts ss
    let closure = Closure a (map idToName args) s'
    return (VarDecl a (idToName f) (Just closure))

translateStmt other = Left $ "Can't translate " ++ show other

translateExprStmt :: Show a => a -> ES.Expression a -> Either String (Stmt a)
translateExprStmt l (ES.AssignExpr _ ES.OpAssign (ES.LVar _ x) e) = do
    e' <- translateExpr e
    return $ Assign l (Name x) e'

translateExprStmt _ other = Left $ "Can't translate " ++ show other

translateExpr :: Show a => ES.Expression a -> Either String (Expr a)
translateExpr = \case
    ES.IntLit _ x    -> return $ PrimLit (PrimInt x)
    ES.BoolLit _ x   -> return $ PrimLit (PrimBool x)
    ES.NullLit _     -> return $ PrimLit PrimNull
    ES.ObjectLit _ m -> do
        props' <- mapM (translateProp . fst) m
        es' <- mapM (translateExpr . snd) m
        return (ObjExpr $ zip props' es')
    ES.VarRef _ x    -> return $ VarExpr (idToName x)
    ES.DotRef _ e i  -> do
        e' <- translateExpr e
        return (GetExpr e' (idToName i))
    ES.CallExpr _ e args -> do
        e' <- translateExpr e
        args' <- mapM translateExpr args
        return $ CallExpr e' args'
    ES.FuncExpr l Nothing params stmts -> do
        s <- translateStmts stmts
        return $ Closure l (map idToName params) s
    other -> Left $ "Can't translate Expr " ++ show other

translateVarDecl :: Show a => ES.VarDecl a -> Either String (Stmt a)
translateVarDecl (ES.VarDecl l x mExpr) = do
    mExpr' <- case mExpr of
                Nothing -> return Nothing
                Just e  -> Just <$> translateExpr e
    return $ VarDecl l (idToName x) mExpr'

translateProp :: Show a => ES.Prop a -> Either String Name
translateProp (ES.PropId _ x) = return $ idToName x
translateProp (ES.PropString _ x) = return $ Name x
translateProp other = Left $ "Can't translate Prop " ++ show other

idToName :: ES.Id a -> Name
idToName i = Name $ ES.unId i
