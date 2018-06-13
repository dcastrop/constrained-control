{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Control.Constrained.ArrowFunctor
  ( ArrowFunctor(..)
  ) where

import Data.Singletons.TypeLits
import Control.Constrained.Category
import Control.Constrained.ArrowVector

type FuncC t f a = (Category t, C t a, C t (f a))

class Category t => ArrowFunctor f t where
  amap :: (FuncC t f a, FuncC t f b) => t a b -> t (f a) (f b)

instance ArrowFunctor [] (->) where
  amap = map

instance (KnownNat n, ArrowVector v t) => ArrowFunctor (v n) t where
  amap f = vec (\i -> proj i >>> f)
