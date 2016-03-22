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
process _ _ _ [] s = return s
process labelDict flows ctx ((l1, l2) : wl') state = do
    let updateEnvWith = \f -> M.update f (l1, ctx) state

    env <- M.lookup (l1, ctx) state
    stmt <- M.lookup l2 labelDict

    let state' = case stmt of
            VarDecl _ x ->
                updateEnvWith (Just . bindValue x (VPrim PrimNull))
            _ -> state
    if state == state'
        then process labelDict flows ctx wl' state
        else do
            let wl'' = filter (\(u, v) -> u == l2) $ S.toList flows
            process labelDict flows ctx (wl' ++ wl'') state'
