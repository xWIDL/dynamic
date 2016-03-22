module Core where

import AST
import Flow
import Model
import qualified Data.Map as M
import qualified Data.Set as S

type Ctx label = [label]
type State label = M.Map (label, Ctx label) Env
type WorkList label = [(label, label)]

process :: Label label => M.Map label (Stmt label) -> S.Set (label, label) ->
                          Ctx label -> WorkList label -> State label -> Maybe (State label)
process labelDict flows = process'
    where
        process' _ [] s = return s
        process' ctx ((l1, l2) : wl') state = do
            env <- M.lookup (l1, ctx) state
            stmt <- M.lookup l2 labelDict

            let state' = case stmt of
                    VarDecl _ x -> updateEnvWith (bindValue x (VPrim PrimNull))
                    _ -> state
            if state == state'
                then process' ctx wl' state
                else do
                    let wl'' = filter (\(u, v) -> u == l2) $ S.toList flows
                    process' ctx (wl' ++ wl'') state'
            where
                updateEnvWith f = case M.lookup (l2, ctx) state of
                    Nothing -> M.insert (l2, ctx) (f initEnv) state
                    Just e  -> M.insert (l2, ctx) (f e) state


driver :: Label label => label -> Program label -> Maybe (State label)
driver start prog =
    let flows = flow prog
        labelDict = labelsOf prog
        startTask = (start, initLabel prog)
        initCtx = []
    in  process labelDict flows initCtx (startTask : S.toList flows)
                (M.singleton (start, initCtx) initEnv)
