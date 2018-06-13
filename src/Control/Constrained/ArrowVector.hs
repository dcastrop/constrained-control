{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Constrained.ArrowVector
  ( ArrowVector(..)
  , VecC
  ) where

import Prelude hiding ( (.), id )

import Control.Constrained.Category
import Control.Constrained.Arrow
import Data.Type.Vector ( Vec )
import qualified Data.Type.Vector as Vec
import Data.Type.Mod
import Data.Singletons.TypeLits

type VecC t v n a = (Category t, C t a, KnownNat n, C t (v n a))

class Arrow t => ArrowVector (v :: Nat -> * -> *) t where

  proj :: forall a m. VecC t v m a => TMod m -> t (v m a) a
  -- proj i = arr "proj" (Vec.proj i)

  vec :: forall a b n. (C t a, VecC t v n b)
      => (TMod n -> t a b) -> t a (v n b)

instance ArrowVector Vec (->) where
  proj = Vec.proj
  vec = Vec.vec
