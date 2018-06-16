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

type VecC cc t v n a = (Category cc t, cc a, KnownNat n, cc (v n a))

class Arrow cc t => ArrowVector cc (v :: Nat -> * -> *) t where

  proj :: forall a m. VecC cc t v m a => TMod m -> t (v m a) a

  vec :: forall a b n. (cc a, VecC cc t v n b)
      => (TMod n -> t a b) -> t a (v n b)

instance ArrowVector NoConstraint Vec (->) where
  proj = Vec.proj
  vec = Vec.vec
