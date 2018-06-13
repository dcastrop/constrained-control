{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Constrained.ArrowVector
  ( ArrowVector(..)
  ) where

import Prelude hiding ( (.), id )

import Control.Constrained.Category
import Control.Constrained.Arrow
import Data.Type.Vector ( Vec )
import qualified Data.Type.Vector as Vec
import Data.Type.Mod
import Data.Singletons
import Data.Singletons.TypeLits

class Arrow t => ArrowVector t where

  vecDict :: (C t a, KnownNat n) => CDict t (Vec n a)
  natDict :: KnownNat n => SNat n -> CDict t n

  proj :: forall a m. (C t a, KnownNat m) => TMod m -> t (Vec m a) a
  proj i
    = case natDict sing :: (CDict t m) of
        CDict ->
          case vecDict :: CDict t (Vec m a) of
            CDict -> arr "proj" (Vec.proj i)

  vec :: forall a b n. (C t a, C t b, KnownNat n)
      => SNat n -> (TMod n -> t a b) -> t a (Vec n b)

instance ArrowVector (->) where
  vecDict = CDict
  natDict _ = CDict
  proj = Vec.proj
  vec = Vec.vec
