{-|
Module      : Dynamic.Interpret
Description : Abstract Intepretation Engine
-}

{-# LANGUAGE TupleSections, LambdaCase #-}
module Dynamic.Interpret where

import Common
import Primitive
import Dynamic.Defs
import Core.Flow
import Core.Abstract
import JS.AST
import JS.Model
import Language.JS.Platform
import Language.JS.Type

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (forM)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class (liftIO)

-- | Analysis result
type InterpretResult label p = (Either String (GoResult label p, InterpretState label p), String)

-- | Interpret program starting from a specific block
interpret :: (Label label, Abstract p, Hom p Prim) =>
             Bool -> label -> Stmt label -> IO (InterpretResult label p)
interpret connected start prog = do
    let cursor = Cursor TopLevel [] (Edge (start, entryLabel prog)) []
    if connected
        then do
            startSession $ \port -> do
                let initState = InterpretState {
                    _envMap = M.singleton (start, TopLevel, []) initEnv,
                    _platPort = Just port,
                    _labelDict = labelsOf prog,
                    _flows = flow prog,
                    _cursor = cursor,
                    _connected = connected
                }
                runWriterT (runExceptT (runStateT go initState))
        else do
            let initState = InterpretState {
                _envMap = M.singleton (start, TopLevel, []) initEnv,
                _platPort = Nothing,
                _labelDict = labelsOf prog,
                _flows = flow prog,
                _cursor = cursor,
                _connected = connected
            }
            runWriterT (runExceptT (runStateT go initState))

type GoResult label p = Maybe (Value p, M.Map Ref (HeapObject label p), Ref)

go :: (Label label, Abstract p, Hom p Prim) => Interpret label p (GoResult label p)
go = do
    oldState <- _envMap <$> get
    cursor <- _cursor <$> get
    tell $ show (pprintCursor cursor)
    tell $ show (pprintEnvMap oldState) ++ "\n" 
    let (l1, l2) = getEdgePair cursor

    env <- lookupM (l1, _chain cursor, _cstr cursor) oldState

    case _edge cursor of
        Edge _ -> updateEnvWith_ (joinEnv env)
        EnterTry _ catcher -> updateEnvWith_ (joinEnv (env { _catcher = Just catcher}))
        ExitTry _ tryHead  -> do
            envOld <- lookupM (tryHead, _chain cursor, _cstr cursor) oldState
            updateEnvWith_ (joinEnv (env { _catcher = _catcher envOld }))

    labelDict <- _labelDict <$> get
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
                    VPrim _ -> throwError' "Can't set property of primitive"
                    VRef r  -> do
                        o <- loadObj r
                        case o of
                            HObjDict dict   -> do
                                updateEnvWith_ $ insertHObj r (HObjDict (M.insert a exprVal dict))
                                cont oldState Nothing
                            HObjClos _ _ _ _ -> throwError' "Can't set property of closure"
                            HObjTop          -> throwError' "Can't set property of HObjTop"
                    VTop    -> throwError' "Wow, Magic"
            ReturnStmt l mExpr -> case mExpr of
                Nothing -> return Nothing
                Just e  -> do
                    retVal <- interpretExpr l e
                    env' <- (_envMap <$> get) >>= lookupM (l2, _chain cursor, _cstr cursor)
                    partialStore <- reachableFrom retVal
                    return $ Just (retVal, partialStore, _nextRef env')

            -- Imperative Control with Path Sensitivity

            If l e s1 s2 -> do
                val <- interpretExpr l e
                case val of
                    VPrim prim ->
                        case matchBool prim of
                            (Nothing, Nothing) ->
                                throwError' $ "How can something " ++ show prim ++ " be neither True or False?"
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
                let catcherEnv = fromMaybe initEnv (M.lookup (catcher, csc, ccs) oldState)
                                 -- FIXME: Exception sensitivity?
                v <- interpretExpr l e

                modify (\s -> s { _envMap = M.insert (catcher, csc, ccs)
                                  (catcherEnv { _bindings = M.insert caught v (_bindings catcherEnv) })
                                  (_envMap s) })
                cont oldState (Just [Edge (l, catcher)])
            -- XXX: don't really fit in the style, but let's make a quick hack though
            InvokeStmt l e f args -> do
                connected <- _connected <$> get
                if connected
                    then do            
                        interpretExpr l e >>= \case
                            VPlat name -> do
                                port <- fromJust . _platPort <$> get
                                vals <- mapM (interpretExpr l) args
                                reply <- liftIO $ invoke port (LInterface name) f (map (valToJsExpr env) vals)
                                case reply of
                                    Sat _ -> cont oldState Nothing -- throw away the result
                                    Unsat -> throwError "Unsat"
                                    _     -> throwError $ "Unsupported reply: " ++ show reply
                            other -> throwError' $ "can't call on " ++ show other
                    else do
                        liftIO $ putStrLn "[WARNING] Invalid invocation without connection"
                        cont oldState Nothing

            other -> throwError' $ "can't interpret " ++ show other

throwError' :: Show label => String -> Interpret label p a
throwError' x = do
    cursor <- _cursor <$> get
    throwError ("[Error : " ++ show (_edge cursor) ++ ", rest: " ++ show (_next cursor) ++ "] " ++ x)

-- Continue without unwinding the stack
cont :: (Abstract p, Label label, Hom p Prim) =>
        EnvMap label p -> Maybe (WorkList label) -> Interpret label p (GoResult label p)
cont oldState mSucc = do
    cursor <- _cursor <$> get
    let l2 = snd $ getEdgePair cursor
    newState <- _envMap <$> get
    if oldState == newState
        then cont'
        else case mSucc of
            Nothing -> do
                flows <- _flows <$> get
                let wl'' = filter (\case
                                    Edge (u, _) -> u == l2
                                    ExitTry (u, _) _ -> u == l2
                                    EnterTry (u, _) _ -> u == l2)
                                  (S.toList flows)
                appendNext wl''
                cont'
            -- Path sensitivity
            Just succs -> do
                appendNext succs
                cont'
    where cont' = tryForwardCursor >>= \isSucc -> if isSucc then go else return Nothing

-- Local Environment Update
updateEnvWith :: Label label => (Env label p -> Interpret label p (Env label p, a)) -> Interpret label p a
updateEnvWith f = do
    cursor <- _cursor <$> get
    let l2 = snd $ getEdgePair cursor
    let chain = _chain cursor
    let cstr = _cstr cursor
    e <- lookupEnv l2 chain cstr
    (e', a) <- f e --- NOTE: Monadic f
    modify (\s -> s { _envMap = M.insert (l2, chain, cstr) e' (_envMap s)})
    return a

updateEnvWith_ :: Label label => (Env label p -> Env label p) -> Interpret label p ()
updateEnvWith_ f = updateEnvWith $ \e -> return (f e, ())

-- Local and Enclosed Lookup
lookupEnvWith :: Label label => String -> (Env label p -> Maybe a) ->
                                Interpret label p (a, ScopeChain label, CallString label)
lookupEnvWith err sel = do
    cursor <- _cursor <$> get
    let l2 = snd $ getEdgePair cursor
    let chain = _chain cursor
    let cstr = _cstr cursor
    lookupEnvWith' l2 chain cstr
    where
        lookupEnvWith' l chain' cstr' = do
            env <- lookupEnv l chain' cstr'
            case sel env of
                Just a  -> return (a, chain', cstr')
                Nothing -> case (chain', cstr') of
                    (Enclosed cs _ father, _ : cstr'') -> lookupEnvWith' cs father cstr''
                    _ -> throwError' err

valueOf :: Label label => Name -> Interpret label p (Value p)
valueOf foo@(Name "Foo") = return $ VPlat foo
valueOf x = (\(a, _, _) -> a) <$> lookupEnvWith ("can't find value of binding: " ++ show x)
                                                (M.lookup x . _bindings)

loadObj :: Label label => Ref -> Interpret label p (HeapObject label p)
loadObj x = (\(a, _, _) -> a) <$> lookupEnvWith ("can't find object of ref: " ++ show x)
                                                (M.lookup x . _store)

reachableFrom :: Label label => Value p -> Interpret label p (M.Map Ref (HeapObject label p))
reachableFrom (VPrim _) = return M.empty
reachableFrom (VRef r) = do
    o <- loadObj r
    case o of
        HObjDict dict   -> foldr M.union (M.singleton r o) <$> mapM reachableFrom (M.elems dict)
        HObjClos _ _ _ _ -> return $ M.singleton r o
        HObjTop          -> throwError' "FIXME: Wow, Magic!"
reachableFrom VTop = throwError' "FIXME: Wow, Magic!"

-- Expression interpretation with side-effects
interpretExpr :: (Abstract p, Label label, Hom p Prim) =>
                 label -> Expr label -> Interpret label p (Value p)
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
                cursor <- _cursor <$> get
                let chain = _chain cursor
                let cstr = _cstr cursor
                let (l1, l2) = getEdgePair cursor

                bindings' <- M.fromList <$> forM (zip args params) (\(arg, param) -> do
                                v <- interpretExpr l arg
                                return (param, v))

                let _newFlows = flow stmt -- XXX: now used
                let _newLabelDict = labelsOf stmt -- XXX: not used
                env <- (_envMap <$> get) >>= lookupM (l1, chain, cstr)
                modify (\s ->
                            s { _envMap = M.insert (start, boundChain, l : boundCStr)
                                                   (Env bindings' (_store env) (_nextRef env) (_catcher env))
                                                   (_envMap s) })
                                  -- FIXME: Should we just use `env` here?
                mVal <- do
                    let newCursor = Cursor boundChain (l : boundCStr)
                                           (Edge (start, entryLabel stmt)) []
                    changeCursor newCursor
                    go
                case mVal of
                    Just (val, store', refCount') -> do
                        modify (\s ->
                            s { _envMap = M.insert (l2, chain, cstr)
                                 (Env (_bindings env)
                                      (store' `joinStore` _store env)
                                      (refCount' `joinRef` _nextRef env)
                                      (_catcher env))
                                 (_envMap s)})
                        return val
                    Nothing  -> return $ VPrim (hom PUndefined)
            other -> throwError' $ show other ++ " is not closure"
        -- VPlat name -> do
        --     connected <- _connected <$> get
        --     if connected
        --         then do
        --             port <- fromJust . _platPort <$> get
        --             vals <- mapM (interpretExpr l) args
        --             reply <- liftIO $ invoke port (LInterface name) f (map (valToJsExpr env) vals)
        --             case reply of
        --                 Sat Nothing -> return $ VPrim (hom PUndefined)
        --                 Replies primTy assertResults ->
        --                     case primTy of
        --                         PTyInt    -> return $ VPrim (hom (reflect (Proxy :: Proxy ANum) assertResults))
        --                         PTyDouble -> return $ VPrim (hom (reflect (Proxy :: Proxy ANum) assertResults))
        --                         PTyString -> return $ VPrim (hom (reflect (Proxy :: Proxy AString) assertResults))
        --                         PTyBool   -> return $ VPrim (hom (reflect (Proxy :: Proxy ABool) assertResults))
        --                         _ -> throwError $ "Confusing primty: " ++ show primTy
        --                 Unsat -> throwError "Unsat"
        --                 _     -> throwError $ "Unsupported reply: " ++ show reply
        --                 other -> throwError' $ "can't call on " ++ show other
        --         else do
        --             liftIO $ putStrLn "[WARNING] Invalid invocation without connection"
        --             cont oldState Nothing
        other -> throwError' $ show other ++ " is not closure"

interpretExpr l (Closure start args stmt) = do
    cursor <- _cursor <$> get
    let chain = _chain cursor
    let cstr = _cstr cursor
    ref <- updateEnvWith $ return <$> storeObj (HObjClos (Enclosed l start chain) cstr args stmt)
    return $ VRef ref
