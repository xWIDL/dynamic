{-# LANGUAGE FlexibleContexts, TupleSections, LambdaCase #-}
module Interpret where

import AST
import Flow
import Model
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

type InterpretState label = M.Map (label, ScopeChain label, CallString label) (Env label)
type WorkList label = [(label, label)]

type Interpret label = StateT (InterpretState label) (ExceptT String (Writer String))

interpret :: Label a => a -> Stmt a->
            (Either String (Maybe (Value a, M.Map Ref (Object a), Ref), InterpretState a)
            , String)
interpret start prog =
    let flows = flow prog
        labelDict = labelsOf prog
        startTask = (start, initLabel prog)
        proc = process labelDict flows TopLevel [] (startTask : S.toList flows)
        (ret, logging) = runWriter (runExceptT (runStateT proc (M.singleton (start, TopLevel, []) initEnv)))
    in  runWriter (runExceptT (runStateT proc (M.singleton (start, TopLevel, []) initEnv)))

process :: Label label => M.Map label (Stmt label) -> S.Set (label, label) ->
                          ScopeChain label -> CallString label -> WorkList label ->
                          Interpret label (Maybe ((Value label), M.Map Ref (Object label), Ref))
process labelDict flows = process'
    where
        process' _ _ [] = return Nothing
        process' chain cstr wl@((l1, l2) : wl') = do
            oldState <- get
            tell $ "========== State ==========\n"
            tell $ "|| chain: " ++ show chain ++ "\n"
            tell $ "|| cstr: " ++ show cstr ++ "\n"
            tell $ "|| wl: " ++ show wl ++ "\n"
            tell $ showState oldState ++ "\n"

            env <- lookupM (l1, chain, cstr) oldState
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
                        env <- get >>= lookupM (l2, chain, cstr)
                        let partialStore = _store env `reachableFrom` retVal
                        return $ Just (retVal, partialStore, _refCount env)
                other -> throwError' $ "can't interpret " ++ show other
            where
                throwError' x = throwError ("[Error : " ++ show l2 ++ ", rest: " ++ show wl' ++ "] " ++ x)

                -- Continue without unwinding the stack
                cont oldState = do
                    newState <- get
                    if oldState == newState
                        then process' chain cstr wl'
                        else do
                            let wl'' = filter (\(u, v) -> u == l2) $ S.toList flows
                            process' chain cstr (wl' ++ wl'')

                -- Local Environment Update
                updateEnvWith f = do
                    e <- lookupState l2 chain cstr
                    (e', a) <- f e --- NOTE: Monadic f
                    modify $ M.insert (l2, chain, cstr) e'
                    return a

                updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

                -- Local and Enclosed Lookup
                lookupEnvWith sel x = lookupEnvWith' l2 chain cstr
                    where
                        lookupEnvWith' l chain cstr = do
                            env <- lookupState l chain cstr
                            case M.lookup x (sel env) of
                                Just a  -> return a
                                Nothing -> case (chain, cstr) of
                                    (Enclosed cs _ father, _ : cstr') -> lookupEnvWith' cs father cstr'
                                    _ -> throwError' $ "Can't find in env: " ++ show x

                valueOf = lookupEnvWith _bindings
                loadObj = lookupEnvWith _store

                -- Expression interpretation with side-effects
                -- NOTE: the "l" here is callsite, maybe we should write it more explicitly
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
                            OClos boundChain@(Enclosed _ start _) boundCStr params stmt -> do
                                bindings' <- M.fromList <$> flip mapM (zip args params) (\(arg, param) -> do
                                                v <- interpret l arg
                                                return (param, v))

                                let newFlows = flow stmt
                                let newLabelDict = labelsOf stmt
                                env <- get >>= lookupM (l1, chain, cstr)
                                modify $ M.insert (start, boundChain, l : boundCStr)
                                                  (Env bindings' (_store env) (_refCount env))
                                                  -- FIXME: Should we just use `env` here?
                                mVal <- process newLabelDict newFlows boundChain (l : boundCStr) ((start, initLabel stmt) : S.toList newFlows)
                                case mVal of
                                    Just (val, store', refCount') -> do
                                        modify $ M.insert (l2, chain, cstr)
                                                 (Env (_bindings env)
                                                      (store' `unionStore` (_store env))
                                                      (refCount' `unionRef` (_refCount env)))
                                        return $ val
                                    Nothing  -> return $ VPrim PrimUndefined
                            other -> throwError' $ show other ++ " is not closure"
                        other -> throwError' $ show other ++ " is not closure"

                interpret l clos@(Closure start args stmt) = do
                    ref <- updateEnvWith $ return <$> storeObj (OClos (Enclosed l start chain) cstr args stmt)
                    return $ VRef ref



lookupState :: Label l => l -> ScopeChain l -> CallString l -> Interpret l (Env l)
lookupState l chain cstr = do
    s <- get
    case M.lookup (l, chain, cstr) s of
        Nothing -> return initEnv
        Just e  -> return e

lookupM :: (MonadError String m, Show k, Ord k) => k -> M.Map k v -> m v
lookupM k m = case M.lookup k m of
    Just v  -> return v
    Nothing -> throwError $ "Can't find " ++ show k

showState :: Show l => InterpretState l -> String
showState = unlines . map (\((l, sc, cstr), env) -> "--------- Env " ++ show l ++ "----------\n" ++
                                                    "Scope Chain: " ++ show sc ++ "\n" ++
                                                    "Call String: "  ++ show cstr ++ "\n" ++
                                                    "Environment: \n" ++ show env ++ "\n"
                          ) . M.toList
