{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Control.Constrained.ArrowFunctor
  ( ArrowFunctor(..)
  ) where

import Data.Singletons
import Data.Singletons.TypeLits
import Data.Type.Vector ( Vec )
import Control.Constrained.Category
import Control.Constrained.ArrowVector

class Category t => ArrowFunctor f t where
  fdict :: C t a => CDict t (f a)
  amap :: (C t a, C t b) => t a b -> t (f a) (f b)

instance ArrowFunctor [] (->) where
  fdict = CDict
  amap = map

vmap :: forall t a b n. (C t a, C t b, KnownNat n, ArrowVector t)
     => t a b -> t (Vec n a) (Vec n b)
vmap f =
    case natDict (sing :: SNat n) :: CDict t n of
      CDict -> case vecDict :: CDict t (Vec n a) of
        CDict -> vec (sing :: SNat n) (\i -> proj i >>> f)

instance (KnownNat n, ArrowVector t) => ArrowFunctor (Vec n) t where
  fdict = case natDict (sing :: SNat n) :: CDict t n of
            CDict -> vecDict
  amap = vmap
