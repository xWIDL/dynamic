{-# LANGUAGE FlexibleContexts, TupleSections, LambdaCase #-}
module Interpret where

import AST
import Flow
import Model
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

type InterpretState label = M.Map (label, Ctx label) (Env label)
type WorkList label = [(label, label)]

type Interpret label = ExceptT String (State (InterpretState label))

process :: Label label => M.Map label (Stmt label) -> S.Set (label, label) ->
                          Ctx label -> WorkList label -> Interpret label (Maybe (Value label))
process labelDict flows = process'
    where
        process' _ [] = return Nothing
        process' ctx ((l1, l2) : wl') = do
            oldState <- get
            env <- lookupM (l1, ctx) oldState
            updateEnvWith_ (unionEnv env)
            stmt <- lookupM l2 labelDict
            case stmt of
                VarDecl _ x -> updateEnvWith_ (bindValue x (VPrim PrimNull)) >> cont oldState
                Assign l x expr -> interpret l expr >>= updateEnvWith_ . bindValue x >> cont oldState
                ReturnStmt l mExpr -> case mExpr of
                    Nothing -> return Nothing
                    Just e  -> interpret l e >>= return . Just
                other -> throwError $ "can't interpret " ++ show other
            where
                cont oldState = do
                    newState <- get
                    if oldState == newState
                        then process' ctx wl'
                        else do
                            let wl'' = filter (\(u, v) -> u == l2) $ S.toList flows
                            process' ctx (wl' ++ wl'')

                updateEnvWith f = do
                    e <- lookupState l2 ctx
                    (e', a) <- f e --- NOTE: Monadic f
                    modify $ M.insert (l2, ctx) e'
                    return a

                lookupEnvWith f = lookupState l2 ctx >>= f

                valueOf x = lookupEnvWith $ lookupM x . _bindings

                updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

                interpret _ (PrimLit prim) = return $ VPrim prim
                interpret l (ObjExpr dict) = do
                    obj <- Object . M.fromList <$> mapM (\(name, expr) -> (name,) <$> interpret l expr) dict
                    ref <- updateEnvWith $ return <$> storeObj obj
                    return $ VRef ref
                interpret _ (VarExpr x) = valueOf x
                interpret l (GetExpr expr attr) = do
                    VRef ref <- interpret l expr -- XXX: Exception
                    Object dict <- lookupEnvWith $ lookupM ref . _store
                    lookupM attr dict
                interpret l (InfixExpr e1 op e2) = do
                    v1 <- interpret l e1
                    v2 <- interpret l e2
                    case (v1, v2) of
                        (VPrim (PrimInt i1), VPrim (PrimInt i2)) ->
                            return $ VPrim (PrimInt (applyInfixOp i1 op i2))
                        _ -> return $ VPrim PrimUndefined

                interpret l (CallExpr e args) =
                    interpret l e >>= \case
                        VClos callsite start boundCtx params stmt -> do
                            bindings' <- M.fromList <$> flip mapM (zip args params) (\(arg, param) -> do
                                            v <- interpret l arg
                                            return (param, v))
                            capturedBindings <- _bindings <$> lookupState callsite boundCtx
                            let newFlows = flow stmt
                            let newLabelDict = labelsOf stmt
                            let newCtx = l : boundCtx
                            env <- get >>= lookupM (l1, ctx)
                            modify $ M.insert (start, newCtx)
                                              (Env (bindings' `M.union` capturedBindings) (_store env) (_refCount env))
                            mVal <- process newLabelDict newFlows newCtx ((start, initLabel stmt) : S.toList newFlows)
                            case mVal of
                                Just val -> return $ val
                                Nothing  -> return $ VPrim PrimUndefined
                        other -> throwError $ show other ++ " is not closure"

                interpret l clos@(Closure start args stmt) = return $ VClos l start ctx args stmt

lookupState :: Label l => l -> Ctx l -> Interpret l (Env l)
lookupState l ctx = do
    s <- get
    case M.lookup (l, ctx) s of
        Nothing -> return initEnv
        Just e  -> return e

lookupM :: (MonadError String m, Show k, Ord k) => k -> M.Map k v -> m v
lookupM k m = case M.lookup k m of
    Just v  -> return v
    Nothing -> throwError $ "Can't find " ++ show k

driver :: Label label => label -> Program label -> (Either String (Maybe (Value label)), InterpretState label)
driver start prog =
    let flows = flow prog
        labelDict = labelsOf prog
        startTask = (start, initLabel prog)
        initCtx = []
        proc = process labelDict flows initCtx (startTask : S.toList flows)
    in  runState (runExceptT proc) (M.singleton (start, initCtx) initEnv)
