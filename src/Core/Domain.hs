module Core.Domain where

import Common

import Language.JS.Platform
import Language.JS.Type

binder :: Name
binder = Name "x"

class Domain a where
    domainsOf :: Proxy a -> [JAssert]


makeRel r n p = JRel r (JVal (JVVar n)) (JVal (JVPrim p))

equalsTo = makeRel Equal
notEqualsTo = makeRel NotEqual
lessThan = makeRel LessThan
lessEq   = makeRel LessEq
greaterThan = makeRel GreaterThan
greaterEq = makeRel GreaterEq
