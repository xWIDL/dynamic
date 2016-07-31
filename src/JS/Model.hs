{-|
Module      : JS.Model
Description : JavaScript Runtime Model
-}

{-# LANGUAGE TemplateHaskell #-}
module JS.Model (
  Env(..), initEnv, Value(..), Bindings, Store,
  ScopeChain(..), CallString,
  HeapObject(..), Ref(..),
  storeObj, joinRef, joinStore, insertHObj,
  bindValue, joinEnv, valToJsExpr
) where

import Core.Abstract
import JS.AST
import Language.JS.Type
import Language.JS.Platform (JsExpr(..), JsVal(..))

import qualified Data.Map as M

-- | Call String: Context Sensitivity
type CallString label = [label]

-- | Scope Chain: Closure capturing
data ScopeChain label = TopLevel -- ^ Top level scope
                      | Enclosed {
                          _cons   :: label, -- ^ construction site label
                          _inner  :: label,  -- ^ closure code block's label
                          _father :: (ScopeChain label) -- ^ father scope chain
                        }
                      deriving (Show, Eq, Ord)

-- | Reference to heap-allocated object
newtype Ref = Ref Int deriving (Show, Eq, Ord)

-- | JS value
data Value p = VPrim p        -- ^ Abstract primitive
             | VRef Ref       -- ^ Heap object
             | VPlat Name     -- ^ Platform object
             | VPlatRef JRef  -- ^ Platform heap object
             | VTop           -- ^ Any value, used to model value lattice
             deriving (Show, Eq)

-- FIXME: Actually, [Function] is also an object ... but we don't model this temporarily

-- | Heap-allocated object
data HeapObject label p = HObjDict (M.Map Name (Value p)) -- ^ Dictionary object
                        | HObjClos (ScopeChain label) (CallString label) [Name] (Stmt label) -- ^ Closure objet
                        | HObjTop -- ^ Any heap objet, used to model object lattice
                        deriving (Show, Eq)

-- | Variable binding environment
type Bindings p = M.Map Name (Value p)

-- | Heap bindings environment
type Store label p = M.Map Ref (HeapObject label p)

-- | Runtime Environment
data Env label p = Env {
    _bindings :: Bindings p,
    _store    :: Store label p,
    _nextRef  :: Ref,
    _catcher  :: Maybe (label, Name) -- ^ Optional exception catching site
} deriving (Eq)

instance (Show label, Show p) => Show (Env label p) where
    show env = "refCount: " ++ show (_nextRef env) ++ "\n" ++
               "catcher: " ++ show (_catcher env) ++ "\n" ++
               "bindings:\n" ++ concatMap (\(Name x, v) -> x ++ "\t" ++ show v ++ "\n") (M.toList (_bindings env)) ++
               "store:\n" ++ concatMap (\(Ref i, o) -> show i ++ "\t" ++ show o ++ "\n") (M.toList (_store env))

-- | Initial environment
initEnv :: Env label p
initEnv = Env M.empty M.empty (Ref 0) Nothing

-- | Bind a value to environment
bindValue :: Name -> Value p -> Env label p -> Env label p
bindValue x v env = env { _bindings = M.insert x v (_bindings env) }

-- | Allocate a heap object in environment
storeObj :: HeapObject label p -> Env label p -> (Env label p, Ref)
storeObj o env =
    let ref@(Ref refi) = _nextRef env
    in  (env { _store    = M.insert ref o (_store env),
               _nextRef = Ref (refi + 1) }, ref)

-- | Insert a heap object indexed by reference, used to update or so
insertHObj :: Ref -> HeapObject label p -> Env label p -> Env label p
insertHObj r o env = env { _store = M.insert r o (_store env) }

joinEnv :: (Eq label, Lattice p) => Env label p -> Env label p -> Env label p
joinEnv (Env b1 s1 rc1 c) (Env b2 s2 rc2 _) =
    Env (b1 `joinBindings` b2) (s1 `joinStore` s2) (rc1 `joinRef` rc2) c

joinBindings :: Lattice p => Bindings p -> Bindings p -> Bindings p
joinBindings = M.unionWith joinValue

joinStore :: (Eq label, Eq p) => Store label p -> Store label p -> Store label p
joinStore = M.unionWith joinHeapObject

joinValue :: Lattice p => Value p -> Value p -> Value p
joinValue (VPrim p1) (VPrim p2) = VPrim $ join p1 p2
joinValue v1 v2 = if v1 == v2 then v1 else VTop -- FIXME: Wow, Magic!

joinHeapObject :: (Eq label, Eq p) => HeapObject label p -> HeapObject label p -> HeapObject label p
joinHeapObject o1 o2 = if o1 == o2 then o1 else HObjTop -- FIXME: Wow, So Magic!

joinRef :: Ref -> Ref -> Ref
joinRef (Ref i1) (Ref _i2) = Ref i1 -- FIXME: Seriously?

-- | Value to Platform JS Expression
valToJsExpr :: (Show p, Show label) => Hom p Prim => Env label p -> Value p -> JsExpr
valToJsExpr env = \case
  VPrim p     -> JVal (JVPrim (hom p :: Prim))
  VPlatRef r  -> JVal (JVRef r)
  VPlat n     -> JInterface n
  VRef r      -> refToJsExpr env r
  other       -> error $ "Doesn't support valToJsExpr of " ++ show other

refToJsExpr :: (Show p, Show label) => Env label p -> Ref -> JsExpr
refToJsExpr e r =
  case M.lookup r (_store e) of
    Just (HObjClos _ _ names _) -> JVal (JVClos (length names))
    Just o -> error $ "No refToJsExpr support for " ++ show o
    Nothing -> error $ "Invalid ref: " ++ show r
