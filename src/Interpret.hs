-- Interpret: Abstract Intepretation Engine

{-# LANGUAGE TupleSections, LambdaCase #-}
module Interpret where

import AST
import Core.Flow
import Model
import Common
import Core.Abstract

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

class (Lattice p, Show p, Reduce p InfixOp, Hom Prim p) => Abstract p where

type InterpretState label p = M.Map (label, ScopeChain label, CallString label) (Env label p)
type WorkList label = [(label, label)]

type Interpret label p = StateT (InterpretState label p) (ExceptT String (Writer String))

interpret :: (Label a, Abstract p) => a -> Stmt a->
            (Either String (Maybe (Value a p, M.Map Ref (Object a p), Ref), InterpretState a p)
            , String)
interpret start prog =
    let flows = flow prog
        labelDict = labelsOf prog
        startTask = (start, initLabel prog)
        proc = process labelDict flows TopLevel [] (startTask : S.toList flows)
        (ret, logging) = runWriter (runExceptT (runStateT proc (M.singleton (start, TopLevel, []) initEnv)))
    in  runWriter (runExceptT (runStateT proc (M.singleton (start, TopLevel, []) initEnv)))

process :: (Label label, Abstract p) =>
           M.Map label (Stmt label) -> S.Set (label, label) ->
           ScopeChain label -> CallString label -> WorkList label ->
           Interpret label p (Maybe ((Value label p), M.Map Ref (Object label p), Ref))
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
                            Nothing -> return $ VPrim (hom PrimNull)
                            Just e  -> interpret l e
                    updateEnvWith_ (bindValue x val) >> cont oldState
                Assign l (LVar x) expr -> interpret l expr >>= updateEnvWith_ . bindValue x >> cont oldState
                Assign l (LProp e a) expr -> do
                    exprVal <- interpret l expr
                    v <- interpret l e
                    case v of
                        VPrim _ -> error "Can't set property of primitive"
                        VRef r  -> do
                            o <- loadObj r
                            case o of
                                Object dict   -> do
                                    updateEnvWith_ $ updateObj r (Object (M.insert a exprVal dict))
                                    cont oldState
                                OClos _ _ _ _ -> error "Can't set property of closure"
                                OTop          -> throwError' "Can't set property of OTop"
                        VTop    -> error "Wow, Magic"
                ReturnStmt l mExpr -> case mExpr of
                    Nothing -> return Nothing
                    Just e  -> do
                        retVal <- interpret l e
                        env <- get >>= lookupM (l2, chain, cstr)
                        partialStore <- reachableFrom retVal
                        return $ Just (retVal, partialStore, _refCount env)

                -- Imperative Control
                If l e s1 s2 -> do
                    _ <- interpret l e -- FIXME: Here, we don't have any path sensitivity .... it might not be precise
                                       --        The only reason for interpretation is to introduce potential side-effects
                    cont oldState

                Skip _ -> cont oldState

                While l e _ -> do
                    _ <- interpret l e
                    cont oldState -- XXX: How to prove that, it *will* halt?
                BreakStmt _ -> cont oldState
                ContStmt a -> cont oldState

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

                reachableFrom (VPrim _) = return M.empty
                reachableFrom (VRef r) = do
                    o <- loadObj r
                    case o of
                        Object dict   -> foldr M.union (M.singleton r o) <$> mapM reachableFrom (M.elems dict)
                        OClos _ _ _ _ -> return $ M.singleton r o
                        OTop          -> error "FIXME: Wow, Magic!"
                reachableFrom VTop     = error "FIXME: Wow, Magic!"

                -- Expression interpretation with side-effects
                -- NOTE: the "l" here is callsite, maybe we should write it more explicitly
                interpret _ (PrimLit prim) = return $ VPrim (hom prim)
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
                            return $ VPrim (reduce op p1 p2)
                        _ -> return $ VPrim (hom PrimUndefined)

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
                                        return val
                                    Nothing  -> return $ VPrim (hom PrimUndefined)
                            other -> throwError' $ show other ++ " is not closure"
                        other -> throwError' $ show other ++ " is not closure"

                interpret l clos@(Closure start args stmt) = do
                    ref <- updateEnvWith $ return <$> storeObj (OClos (Enclosed l start chain) cstr args stmt)
                    return $ VRef ref

lookupState :: Label l => l -> ScopeChain l -> CallString l -> Interpret l p (Env l p)
lookupState l chain cstr = do
    s <- get
    case M.lookup (l, chain, cstr) s of
        Nothing -> return initEnv
        Just e  -> return e

showState :: (Show l, Abstract p) => InterpretState l p -> String
showState = unlines . map (\((l, sc, cstr), env) -> "--------- Env " ++ show l ++ "----------\n" ++
                                                    "Scope Chain: " ++ show sc ++ "\n" ++
                                                    "Call String: "  ++ show cstr ++ "\n" ++
                                                    "Environment: \n" ++ show env ++ "\n"
                          ) . M.toList
