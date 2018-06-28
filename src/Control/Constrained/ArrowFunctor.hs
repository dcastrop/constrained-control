{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE IncoherentInstances #-}
module Control.Constrained.ArrowFunctor
  ( ArrowFunctor(..)
  ) where

import Data.Singletons.TypeLits
import Control.Constrained.Category
import Control.Constrained.ArrowVector

type FuncC cc t f a = (Category cc t, cc a, cc (f a))

class Category cc t => ArrowFunctor cc f t where
  amap :: (FuncC cc t f a, FuncC cc t f b) => t a b -> t (f a) (f b)

instance (KnownNat n, ArrowVector cc v t) => ArrowFunctor cc (v n) t where
  amap f = vec (\i -> proj i >>> f)

instance Functor f => ArrowFunctor NoConstraint f (->) where
  amap = fmap
