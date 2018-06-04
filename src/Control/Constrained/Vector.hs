{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Constrained.Vector
  ( Idx
  , idx
  , Vec
  , vec
  , proj
  , len
  , ArrowVector(..)
  ) where

import Prelude hiding ( (.), id )

import Control.Constrained.Category
import Control.Constrained.Arrow
import Data.Typeable
import Data.Type.Natural
import Data.Word

-- | Vectors
--
newtype Idx (n :: Nat) = Idx Word32

deriving instance Typeable (Idx n)

instance Show (Idx n) where
  show (Idx w) = show w

idx :: Leq n m -> SNat n -> Idx m
idx _ n = Idx $ sNatToInt n

destruct :: (Idx 'Z -> a) -> (Idx ('S n) -> a) -> Idx m -> a
destruct f g (Idx w) | w <= 0 = f (Idx w)
                     | otherwise = g (Idx w)

newtype Vec (n :: Nat) a = Vec [a]

deriving instance Typeable 'Z
deriving instance Typeable n => Typeable ('S n)
deriving instance Typeable n => Typeable (Vec n)

instance Functor (Vec n) where
  fmap f (Vec xs) = Vec (fmap f xs)

vec :: SNat n -> ((Word32, a) -> b) -> a -> Vec n b
vec n f x = Vec $ map (f . (,x)) [0..w-1]
  where
    w = sNatToInt n

len :: Vec n a -> Idx n
len (Vec v) = Idx $ fromIntegral $ length v

proj :: Idx n -> Vec ('S n) a -> a
proj (Idx i) (Vec l) = l !! fromIntegral i

instance ArrowVector (->) where
  vecDict = CDict
  zDict = CDict
  sDict = CDict
  vproj = proj
  vmap f (Vec xs) = Vec $ map f xs
  vgen = vec

class Arrow t => ArrowVector t where

  vecDict :: (C t a, C t n) => CDict t (Vec n a)
  zDict :: CDict t 'Z
  sDict :: C t n => CDict t ('S n)

  vproj :: forall a n. (C t a, C t n, C t (Idx n)) => Idx n -> t (Vec ('S n) a) a
  vproj i = case sDict :: (CDict t ('S n)) of
            CDict ->
              case vecDict :: CDict t (Vec ('S n) a) of
                CDict -> arr "proj" (proj i)

  vmap :: forall a b n. (C t a, C t b, C t n, C t Word32, C t (Idx n), SingI n)
       => t a b -> t (Vec n a) (Vec n b)
  vmap f = case ( vecDict :: CDict t (Vec n a)
                , vecDict :: CDict t (Vec n b)
                ) of
             (CDict, CDict) ->
                case ( pairDict :: CDict t (Word32, Vec n a)
                     , pairDict :: CDict t (Idx n, Vec n a) ) of
                  (CDict, CDict) -> vgen i fun
    where
      i :: Idx n
      i = Idx $ sNatToInt (sing :: SNat n)
      fun :: C t (Word32, Vec n a) => t (Word32, Vec n a) b
      fun = nget >>> f
      nget :: C t (Word32, Vec n a) => t (Word32, Vec n a) a
      nget = arr "(proj)" $ \(w, Vec x) -> x !! fromIntegral w

  vgen :: forall a b n. ( C t a, C t b, C t n, C t (Idx n), C t Word32, SingI n )
       => Idx n -> t (Word32, a) b -> t a (Vec n b)
  vgen (Idx w) f = case ( pairDict :: CDict t (Word32, a)) of
                      CDict ->
                        case pairDict :: CDict t (Word32, a) of
                          CDict ->
                            case ( vecDict :: CDict t (Vec n (Word32, a))
                              , vecDict :: CDict t (Vec n b)) of
                              (CDict, CDict) -> repl >>> vmap f
    where
      repl :: (C t a, C t (Vec n (Word32, a)))
           => t a (Vec n (Word32, a))
      repl = arr "(repl)" $ \x -> Vec [(i, x) | i <- [0..w-1]]
