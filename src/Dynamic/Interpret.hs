{-|
Module      : Dynamic.Interpret
Description : Abstract Intepretation Engine
-}

{-# LANGUAGE TupleSections, LambdaCase #-}
module Dynamic.Interpret (interpret, InterpretResult) where

import Common
import Dynamic.Defs
import Core.Flow
import Core.Abstract
import JS.AST
import JS.Model
import JS.Platform
import JS.Type

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class (liftIO)

-- | Analysis result
type InterpretResult label p = (Either String (Maybe (Value p, M.Map Ref (HeapObject label p),
                                                      Ref),
                                               InterpretState label p), String)

-- | Interpret program starting from a specific block
interpret :: (Label label, Abstract p, Hom p Prim) => label -> Stmt label -> IO (InterpretResult label p)
interpret start prog = do
    let flows = flow prog
    let labelDict = labelsOf prog
    let startTask = Edge (start, entryLabel prog)
    let proc = process labelDict flows TopLevel [] ([startTask]) -- : S.toList flows)
    startSession $ \port -> do
        let initState = InterpretState {
            _envMap = M.singleton (start, TopLevel, []) initEnv,
            _platPort = port
        }
        runWriterT (runExceptT (runStateT proc initState))

process :: (Label label, Abstract p, Hom p Prim) =>
           M.Map label (Stmt label) -> S.Set (Edge label) ->
           ScopeChain label -> CallString label -> WorkList label ->
           Interpret label p (Maybe ((Value p), M.Map Ref (HeapObject label p), Ref))
process labelDict flows = process'
    where
        process' _ _ [] = return Nothing
        process' chain cstr wl@(work : wl') = do
            oldState <- _envMap <$> get
            tell $ "========== State ==========\n"
            tell $ "|| chain: " ++ show chain ++ "\n"
            tell $ "|| cstr: " ++ show cstr ++ "\n"
            tell $ "|| wl: " ++ show wl ++ "\n"
            tell $ printEnvMap oldState ++ "\n" 

            env <- lookupM (l1, chain, cstr) oldState

            case work of
                Edge _ -> updateEnvWith_ (unionEnv env)
                EnterTry _ catcher -> updateEnvWith_ (unionEnv (env { _catcher = Just catcher}))
                ExitTry _ tryHead  -> do
                    envOld <- lookupM (tryHead, chain, cstr) oldState
                    updateEnvWith_ (unionEnv (env { _catcher = _catcher envOld }))

            case M.lookup l2 labelDict of
                Nothing -> cont oldState Nothing -- FIXME: Maybe we should give catch a label and make it stmt? Not clear
                Just stmt -> case stmt of
                    VarDecl l x mExpr -> do
                        val <- case mExpr of
                                Nothing -> return $ VPrim (hom PNull)
                                Just e  -> interpretExpr l e
                        updateEnvWith_ (bindValue x val) >> cont oldState Nothing
                    Assign l (LVar x) expr -> interpretExpr l expr >>= updateEnvWith_ . bindValue x >> cont oldState Nothing
                    Assign l (LAttr e a) expr -> do
                        exprVal <- interpretExpr l expr
                        v <- interpretExpr l e
                        case v of
                            VPrim _ -> error "Can't set property of primitive"
                            VRef r  -> do
                                o <- loadObj r
                                case o of
                                    HObjDict dict   -> do
                                        updateEnvWith_ $ insertHObj r (HObjDict (M.insert a exprVal dict))
                                        cont oldState Nothing
                                    HObjClos _ _ _ _ -> error "Can't set property of closure"
                                    HObjTop          -> throwError' "Can't set property of HObjTop"
                            VTop    -> error "Wow, Magic"
                    ReturnStmt l mExpr -> case mExpr of
                        Nothing -> return Nothing
                        Just e  -> do
                            retVal <- interpretExpr l e
                            env' <- (_envMap <$> get) >>= lookupM (l2, chain, cstr)
                            partialStore <- reachableFrom retVal
                            return $ Just (retVal, partialStore, _nextRef env')

                    -- Imperative Control with Path Sensitivity

                    If l e s1 s2 -> do
                        val <- interpretExpr l e
                        case val of
                            VPrim prim ->
                                case matchBool prim of
                                    (Nothing, Nothing) ->
                                        error $ "How can something " ++ show prim ++
                                                " be neither True or False?"
                                    (Just _tpv, Nothing) ->
                                        cont oldState (Just [Edge (l, entryLabel s1)])
                                    (Nothing, Just _fpv) ->
                                        cont oldState (Just [Edge (l, entryLabel s2)])
                                    (Just _tpv, Just _fpv) -> cont oldState Nothing
                                    -- FIXME: even we got the restricted primitive value,
                                    --        it is still awkward to use. It is not easy even
                                    --        the VarExpr case, since we don't have a way to
                                    --        prevent the more general case of binding to overwrite
                                    --        our restriction.

                            _ -> cont oldState Nothing

                    Skip _ -> cont oldState Nothing

                    While l e _ -> do
                        _ <- interpretExpr l e
                        cont oldState Nothing -- XXX: How to prove that, it *will* halt?
                    BreakStmt _ -> cont oldState Nothing
                    ContStmt _a -> cont oldState Nothing
                    TryStmt _ _ _ _ -> cont oldState Nothing
                    ThrowStmt l e -> do
                        ((catcher, caught), csc, ccs) <- lookupEnvWith "can't find catcher" _catcher
                        let catcherEnv = case M.lookup (catcher, csc, ccs) oldState of
                                            Nothing -> initEnv
                                            Just e' -> e' -- FIXME: Exception sensitivity?
                        v <- interpretExpr l e

                        modify (\s -> s { _envMap = M.insert (catcher, csc, ccs)
                                          (catcherEnv { _bindings = M.insert caught v (_bindings catcherEnv) })
                                          (_envMap s) })
                        cont oldState (Just [Edge (l, catcher)])
                    -- XXX: don't really fit in the style, but let's make a quick hack though
                    InvokeStmt l e f args -> interpretExpr l e >>= \case
                        VPlat name -> do
                            port <- _platPort <$> get
                            vals <- mapM (interpretExpr l) args
                            reply <- liftIO $ invoke port (LInterface name) f (map valToJsExpr vals)
                            case reply of
                                Sat -> cont oldState Nothing
                                Unsat -> throwError "Unsat"
                        other -> throwError' $ "can't call on " ++ show other

                    other -> throwError' $ "can't interpret " ++ show other
            where
                (l1, l2) = case work of
                            Edge p -> p
                            ExitTry p _ -> p
                            EnterTry p _ -> p

                throwError' x = throwError ("[Error : " ++ show l2 ++ ", rest: " ++ show wl' ++ "] " ++ x)

                -- Continue without unwinding the stack
                cont oldState mSucc = do
                    newState <- _envMap <$> get
                    if oldState == newState
                        then process' chain cstr wl'
                        else case mSucc of
                            Nothing -> do
                                let wl'' = filter (\case
                                                    Edge (u, _) -> u == l2
                                                    ExitTry (u, _) _ -> u == l2
                                                    EnterTry (u, _) _ -> u == l2)
                                                  (S.toList flows)
                                process' chain cstr (wl' ++ wl'')
                            -- Path sensitivity
                            Just succs -> process' chain cstr (wl' ++ succs)

                -- Local Environment Update
                updateEnvWith f = do
                    e <- lookupEnv l2 chain cstr
                    (e', a) <- f e --- NOTE: Monadic f
                    modify (\s -> s { _envMap = M.insert (l2, chain, cstr) e' (_envMap s)})
                    return a

                updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

                -- addBinding l chain' cstr' x v = do
                --     e <- lookupEnv l chain' cstr'
                --     modify (\s -> s { _envMap = M.insert (l, chain, cstr') (e { _bindings = M.insert x v (_bindings e) }) (_envMap s)})

                -- Local and Enclosed Lookup
                lookupEnvWith err sel = lookupEnvWith' l2 chain cstr
                    where
                        lookupEnvWith' l chain' cstr' = do
                            env <- lookupEnv l chain' cstr'
                            case sel env of
                                Just a  -> return (a, chain', cstr')
                                Nothing -> case (chain', cstr') of
                                    (Enclosed cs _ father, _ : cstr'') -> lookupEnvWith' cs father cstr''
                                    _ -> throwError' err

                valueOf foo@(Name "Foo") = return $ VPlat foo
                valueOf x = (\(a, _, _) -> a) <$> lookupEnvWith ("can't find value of binding: " ++ show x)
                                                                (M.lookup x . _bindings)
                loadObj x = (\(a, _, _) -> a) <$> lookupEnvWith ("can't find object of ref: " ++ show x)
                                                                (M.lookup x . _store)

                reachableFrom (VPrim _) = return M.empty
                reachableFrom (VRef r) = do
                    o <- loadObj r
                    case o of
                        HObjDict dict   -> foldr M.union (M.singleton r o) <$> mapM reachableFrom (M.elems dict)
                        HObjClos _ _ _ _ -> return $ M.singleton r o
                        HObjTop          -> error "FIXME: Wow, Magic!"
                reachableFrom VTop     = error "FIXME: Wow, Magic!"

                -- Expression interpretation with side-effects
                -- NOTE: the "l" here is callsite, maybe we should write it more explicitly
                interpretExpr _ (PrimLit prim) = return $ VPrim (hom prim)
                interpretExpr l (ObjExpr dict) = do
                    obj <- HObjDict . M.fromList <$> mapM (\(name, expr) -> (name,) <$> interpretExpr l expr) dict
                    ref <- updateEnvWith $ return <$> storeObj obj
                    return $ VRef ref
                interpretExpr _ (VarExpr x) = valueOf x
                interpretExpr l (GetExpr expr attr) = do
                    VRef ref <- interpretExpr l expr -- XXX: Exception
                    HObjDict dict <- loadObj ref
                    lookupM attr dict
                interpretExpr l (InfixExpr e1 op e2) = do
                    v1 <- interpretExpr l e1
                    v2 <- interpretExpr l e2
                    case (v1, v2) of
                        (VPrim p1, VPrim p2) ->
                            return $ VPrim (reduce op p1 p2)
                        _ -> return $ VPrim (hom PUndefined)

                interpretExpr l (CallExpr e args) =
                    interpretExpr l e >>= \case
                        VRef ref -> loadObj ref >>= \case
                            HObjClos boundChain@(Enclosed _ start _) boundCStr params stmt -> do
                                bindings' <- M.fromList <$> flip mapM (zip args params) (\(arg, param) -> do
                                                v <- interpretExpr l arg
                                                return (param, v))

                                let newFlows = flow stmt
                                let newLabelDict = labelsOf stmt
                                env <- (_envMap <$> get) >>= lookupM (l1, chain, cstr)
                                modify (\s ->
                                            s { _envMap = M.insert (start, boundChain, l : boundCStr)
                                                                   (Env bindings' (_store env) (_nextRef env) (_catcher env))
                                                                   (_envMap s) })
                                                  -- FIXME: Should we just use `env` here?
                                mVal <- process newLabelDict newFlows boundChain (l : boundCStr)
                                                [Edge (start, entryLabel stmt)]
                                case mVal of
                                    Just (val, store', refCount') -> do
                                        modify (\s ->
                                            s { _envMap = M.insert (l2, chain, cstr)
                                                 (Env (_bindings env)
                                                      (store' `unionStore` (_store env))
                                                      (refCount' `unionRef` (_nextRef env))
                                                      (_catcher env))
                                                 (_envMap s)})
                                        return val
                                    Nothing  -> return $ VPrim (hom PUndefined)
                            other -> throwError' $ show other ++ " is not closure"
                        other -> throwError' $ show other ++ " is not closure"

                interpretExpr l (Closure start args stmt) = do
                    ref <- updateEnvWith $ return <$> storeObj (HObjClos (Enclosed l start chain) cstr args stmt)
                    return $ VRef ref
