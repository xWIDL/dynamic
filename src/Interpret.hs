-- Interpret: Abstract Intepretation Engine

{-# LANGUAGE TupleSections, LambdaCase #-}
module Interpret where

import JS.AST
import Core.Flow
import JS.Model
import Common
import Core.Abstract
import JS.Platform
import JS.Type

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class (liftIO)


class (Lattice p, Show p, Reduce p InfixOp, Hom Prim p) => Abstract p where
    matchBool :: p -> (Maybe p, Maybe p)

data InterpretState label p = InterpretState {
    _envMap   :: M.Map (label, ScopeChain label, CallString label) (Env label p),
    _platPort :: PlatPort
}

type WorkList label = [Edge label]

type Interpret label p = StateT (InterpretState label p) (ExceptT String (WriterT String IO))

interpret :: (Label a, Abstract p, Hom p Prim) => a -> Stmt a -> IO
             (Either String (Maybe (Value a p, M.Map Ref (Object a p), Ref), InterpretState a p)
             , String)
interpret start prog = do
    let flows = flow prog
    let labelDict = labelsOf prog
    let startTask = Edge (start, initLabel prog)
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
           Interpret label p (Maybe ((Value label p), M.Map Ref (Object label p), Ref))
process labelDict flows = process'
    where
        process' _ _ [] = return Nothing
        process' chain cstr wl@(work : wl') = do
            oldState <- _envMap <$> get
            tell $ "========== State ==========\n"
            tell $ "|| chain: " ++ show chain ++ "\n"
            tell $ "|| cstr: " ++ show cstr ++ "\n"
            tell $ "|| wl: " ++ show wl ++ "\n"
            tell $ showState oldState ++ "\n"

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
                                Just e  -> interpret l e
                        updateEnvWith_ (bindValue x val) >> cont oldState Nothing
                    Assign l (LVar x) expr -> interpret l expr >>= updateEnvWith_ . bindValue x >> cont oldState Nothing
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
                                        cont oldState Nothing
                                    OClos _ _ _ _ -> error "Can't set property of closure"
                                    OTop          -> throwError' "Can't set property of OTop"
                            VTop    -> error "Wow, Magic"
                    ReturnStmt l mExpr -> case mExpr of
                        Nothing -> return Nothing
                        Just e  -> do
                            retVal <- interpret l e
                            env <- (_envMap <$> get) >>= lookupM (l2, chain, cstr)
                            partialStore <- reachableFrom retVal
                            return $ Just (retVal, partialStore, _refCount env)

                    -- Imperative Control with Path Sensitivity

                    If l e s1 s2 -> do
                        val <- interpret l e
                        case val of
                            VPrim prim ->
                                case matchBool prim of
                                    (Nothing, Nothing) ->
                                        error $ "How can something " ++ show prim ++
                                                " be neither True or False?"
                                    (Just tpv, Nothing) ->
                                        cont oldState (Just [Edge (l, initLabel s1)])
                                    (Nothing, Just fpv) ->
                                        cont oldState (Just [Edge (l, initLabel s2)])
                                    (Just tpv, Just fpv) -> cont oldState Nothing
                                    -- FIXME: even we got the restricted primitive value,
                                    --        it is still awkward to use. It is not easy even
                                    --        the VarExpr case, since we don't have a way to
                                    --        prevent the more general case of binding to overwrite
                                    --        our restriction.

                            _ -> cont oldState Nothing

                    Skip _ -> cont oldState Nothing

                    While l e _ -> do
                        _ <- interpret l e
                        cont oldState Nothing -- XXX: How to prove that, it *will* halt?
                    BreakStmt _ -> cont oldState Nothing
                    ContStmt a -> cont oldState Nothing
                    TryStmt _ _ _ _ -> cont oldState Nothing
                    ThrowStmt l e -> do
                        ((catcher, caught), csc, ccs) <- lookupEnvWith "can't find catcher" _catcher
                        let catcherEnv = case M.lookup (catcher, csc, ccs) oldState of
                                            Nothing -> initEnv
                                            Just e  -> e -- FIXME: Exception sensitivity?
                        v <- interpret l e

                        modify (\s -> s { _envMap = M.insert (catcher, csc, ccs)
                                          (catcherEnv { _bindings = M.insert caught v (_bindings catcherEnv) })
                                          (_envMap s) })
                        cont oldState (Just [Edge (l, catcher)])
                    -- XXX: don't really fit in the style, but let's make a quick hack though
                    InvokeStmt l e f args -> interpret l e >>= \case
                        VPlat name -> do
                            port <- _platPort <$> get
                            vals <- mapM (interpret l) args
                            reply <- liftIO $ invoke port (LInterface name) f (map valToPlatExpr vals)
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
                    e <- lookupState l2 chain cstr
                    (e', a) <- f e --- NOTE: Monadic f
                    modify (\s -> s { _envMap = M.insert (l2, chain, cstr) e' (_envMap s)})
                    return a

                updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

                addBinding l chain cstr x v = do
                    e <- lookupState l chain cstr
                    modify (\s -> s { _envMap = M.insert (l, chain, cstr) (e { _bindings = M.insert x v (_bindings e) }) (_envMap s)})

                -- Local and Enclosed Lookup
                lookupEnvWith err sel = lookupEnvWith' l2 chain cstr
                    where
                        lookupEnvWith' l chain cstr = do
                            env <- lookupState l chain cstr
                            case sel env of
                                Just a  -> return (a, chain, cstr)
                                Nothing -> case (chain, cstr) of
                                    (Enclosed cs _ father, _ : cstr') -> lookupEnvWith' cs father cstr'
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
                        _ -> return $ VPrim (hom PUndefined)

                interpret l (CallExpr e args) =
                    interpret l e >>= \case
                        VRef ref -> loadObj ref >>= \case
                            OClos boundChain@(Enclosed _ start _) boundCStr params stmt -> do
                                bindings' <- M.fromList <$> flip mapM (zip args params) (\(arg, param) -> do
                                                v <- interpret l arg
                                                return (param, v))

                                let newFlows = flow stmt
                                let newLabelDict = labelsOf stmt
                                env <- (_envMap <$> get) >>= lookupM (l1, chain, cstr)
                                modify (\s ->
                                            s { _envMap = M.insert (start, boundChain, l : boundCStr)
                                                                   (Env bindings' (_store env) (_refCount env) (_catcher env))
                                                                   (_envMap s) })
                                                  -- FIXME: Should we just use `env` here?
                                mVal <- process newLabelDict newFlows boundChain (l : boundCStr)
                                                [Edge (start, initLabel stmt)]
                                case mVal of
                                    Just (val, store', refCount') -> do
                                        modify (\s ->
                                            s { _envMap = M.insert (l2, chain, cstr)
                                                 (Env (_bindings env)
                                                      (store' `unionStore` (_store env))
                                                      (refCount' `unionRef` (_refCount env))
                                                      (_catcher env))
                                                 (_envMap s)})
                                        return val
                                    Nothing  -> return $ VPrim (hom PUndefined)
                            other -> throwError' $ show other ++ " is not closure"
                        other -> throwError' $ show other ++ " is not closure"

                interpret l clos@(Closure start args stmt) = do
                    ref <- updateEnvWith $ return <$> storeObj (OClos (Enclosed l start chain) cstr args stmt)
                    return $ VRef ref

lookupState :: Label l => l -> ScopeChain l -> CallString l -> Interpret l p (Env l p)
lookupState l chain cstr = do
    s <- _envMap <$> get
    case M.lookup (l, chain, cstr) s of
        Nothing -> return initEnv
        Just e  -> return e

showState :: (Show l, Abstract p) => M.Map (l, ScopeChain l, CallString l) (Env l p) -> String
showState = unlines . map (\((l, sc, cstr), env) -> "--------- Env " ++ show l ++ "----------\n" ++
                                                    "Scope Chain: " ++ show sc ++ "\n" ++
                                                    "Call String: "  ++ show cstr ++ "\n" ++
                                                    "Environment: \n" ++ show env ++ "\n"
                          ) . M.toList


valToPlatExpr :: Hom p Prim => Value a p -> JsExpr
valToPlatExpr (VPrim p)    = JVal (JVPrim (hom p :: Prim))
valToPlatExpr (VPlatRef r) = JVal (JVRef r)
