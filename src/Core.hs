module Core where

import AST
import Flow
import Model
import qualified Data.Map as M

type Ctx label = [label]
type State label = M.Map (label, Ctx label) Env
