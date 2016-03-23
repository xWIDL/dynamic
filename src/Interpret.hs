{-# LANGUAGE FlexibleContexts, TupleSections, LambdaCase #-}
module Interpret where

import AST
import Flow
import Model
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

type InterpretState label = M.Map (label, ScopeChain label) (Env label)
type WorkList label = [(label, label)]

type Interpret label = ExceptT String (State (InterpretState label))

process :: Label label => M.Map label (Stmt label) -> S.Set (label, label) ->
                          ScopeChain label -> WorkList label ->
                          Interpret label (Maybe ((Value label), M.Map Ref (Object label), Ref))
process labelDict flows = process'
    where
        process' _ [] = return Nothing
        process' chain ((l1, l2) : wl') = do
            oldState <- get
            env <- lookupM (l1, chain) oldState
            updateEnvWith_ (unionEnv env)
            stmt <- lookupM l2 labelDict
            case stmt of
                VarDecl l x mExpr -> do
                    val <- case mExpr of
                            Nothing -> return $ VPrim PrimNull
                            Just e  -> interpret l e
                    updateEnvWith_ (bindValue x val) >> cont oldState
                Assign l x expr -> interpret l expr >>= updateEnvWith_ . bindValue x >> cont oldState
                ReturnStmt l mExpr -> case mExpr of
                    Nothing -> return Nothing
                    Just e  -> do
                        retVal <- interpret l e
                        env <- get >>= lookupM (l2, chain)
                        let partialStore = _store env `reachableFrom` retVal
                        return $ Just (retVal, partialStore, _refCount env)
                other -> throwError $ "can't interpret " ++ show other
            where
                -- Continue without unwinding the stack
                cont oldState = do
                    newState <- get
                    if oldState == newState
                        then process' chain wl'
                        else do
                            let wl'' = filter (\(u, v) -> u == l2) $ S.toList flows
                            process' chain (wl' ++ wl'')

                -- Local Environment Update
                updateEnvWith f = do
                    e <- lookupState l2 chain
                    (e', a) <- f e --- NOTE: Monadic f
                    modify $ M.insert (l2, chain) e'
                    return a

                updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

                -- Local and Enclosed Lookup
                lookupEnvWith sel x = lookupEnvWith' l2 chain
                    where
                        lookupEnvWith' l chain = do
                            env <- lookupState l chain
                            case M.lookup x (sel env) of
                                Just a  -> return a
                                Nothing -> case chain of
                                    TopLevel -> throwError $ "Can't find in env " ++ show x
                                    Enclosed cs _ father -> lookupEnvWith' cs father --- FIXME: Not sure about this

                valueOf = lookupEnvWith _bindings
                loadObj = lookupEnvWith _store

                -- Expression interpretation with side-effects
                interpret _ (PrimLit prim) = return $ VPrim prim
                interpret l (ObjExpr dict) = do
                    obj <- Object . M.fromList <$> mapM (\(name, expr) -> (name,) <$> interpret l expr) dict
                    ref <- updateEnvWith $ return <$> storeObj obj
                    return $ VRef ref
                interpret _ (VarExpr x) = valueOf x
                interpret l (GetExpr expr attr) = do
                    VRef ref <- interpret l expr -- XXX: Exception
                    Object dict <- loadObj ref
                    lookupM attr dict
                interpret l (InfixExpr e1 op e2) = do
                    v1 <- interpret l e1
                    v2 <- interpret l e2
                    case (v1, v2) of
                        (VPrim p1, VPrim p2) ->
                            return $ VPrim (applyInfixOp p1 op p2)
                        _ -> return $ VPrim PrimUndefined

                interpret l (CallExpr e args) =
                    interpret l e >>= \case
                        VRef ref -> loadObj ref >>= \case
                            OClos boundChain@(Enclosed _ start _) params stmt -> do
                                bindings' <- M.fromList <$> flip mapM (zip args params) (\(arg, param) -> do
                                                v <- interpret l arg
                                                return (param, v))

                                let newFlows = flow stmt
                                let newLabelDict = labelsOf stmt
                                env <- get >>= lookupM (l1, chain)
                                modify $ M.insert (start, boundChain)
                                                  (Env bindings' (_store env) (_refCount env))
                                                  -- FIXME: Should we just use `env` here?
                                mVal <- process newLabelDict newFlows boundChain ((start, initLabel stmt) : S.toList newFlows)
                                case mVal of
                                    Just (val, store', refCount') -> do
                                        modify $ M.insert (l2, chain)
                                                 (Env (_bindings env)
                                                      (store' `unionStore` (_store env))
                                                      (refCount' `unionRef` (_refCount env)))
                                        return $ val
                                    Nothing  -> return $ VPrim PrimUndefined
                            other -> throwError $ show other ++ " is not closure"
                        other -> throwError $ show other ++ " is not closure"

                interpret l clos@(Closure start args stmt) = do
                    ref <- updateEnvWith $ return <$> storeObj (OClos (Enclosed l start chain) args stmt)
                    return $ VRef ref


lookupState :: Label l => l -> ScopeChain l -> Interpret l (Env l)
lookupState l chain = do
    s <- get
    case M.lookup (l, chain) s of
        Nothing -> return initEnv
        Just e  -> return e

lookupM :: (MonadError String m, Show k, Ord k) => k -> M.Map k v -> m v
lookupM k m = case M.lookup k m of
    Just v  -> return v
    Nothing -> throwError $ "Can't find " ++ show k

-- driver :: Label label => label -> Program label -> (Either String (Maybe (Value label)), InterpretState label)
driver start prog =
    let flows = flow prog
        labelDict = labelsOf prog
        startTask = (start, initLabel prog)
        proc = process labelDict flows TopLevel (startTask : S.toList flows)
    in  runState (runExceptT proc) (M.singleton (start, TopLevel) initEnv)
