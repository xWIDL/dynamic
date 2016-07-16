{-|
Module      : Common
Description : Some common utilities
-}

{-# LANGUAGE PolyKinds #-}

module Common (lookupM, Proxy(..)) where

import Control.Monad.Except
import qualified Data.Map as M

-- | lookup a map and fail in place if nothing is found
lookupM :: (MonadError String m, Show k, Ord k) => k -> M.Map k v -> m v
lookupM k m = case M.lookup k m of
    Just v  -> return v
    Nothing -> throwError $ "Can't find " ++ show k

-- | Type proxy
data Proxy t = Proxy
