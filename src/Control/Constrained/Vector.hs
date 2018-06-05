{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.Constrained.Vector
  ( Vec
  , Mod(..)
  , enumerateM
  , toNat
  , vempty
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

-- | Vectors
--

newtype Vec (n :: Nat) a = Vec [a]

instance Show (Vec 'Z a) where
  show _ = "[]"

deriving instance Typeable 'Z
deriving instance Typeable n => Typeable ('S n)
deriving instance Typeable n => Typeable (Vec n)

instance Functor (Vec n) where
  fmap f (Vec xs) = Vec (fmap f xs)

vempty :: Vec 'Z a
vempty = Vec []

data Mod (m :: Nat) where
  ModS :: Leq n m -> Mod ('S m)

instance Show (Mod m) where
  show (ModS n) = show $ toInt $ toNat n
    where
      toInt :: SNat n -> Int
      toInt r = sNatToInt r

test :: SNat n -> SNat m -> Maybe (Leq n m)
test SZ m = Just $ ZeroLeq m
test _ SZ = Nothing
test (SS n) (SS m) = fmap SuccLeqSucc $ test n m

succMod :: Mod b -> Mod b
succMod (ModS l) =
  case test (SS $ toNat l) b of
    Just f -> ModS f
    Nothing -> ModS $ ZeroLeq b
  where
    b = boundOf l

boundOf :: Leq a b -> SNat b
boundOf (ZeroLeq b) = b
boundOf (SuccLeqSucc b) = SS (boundOf b)

plusLeq :: SingI b => SNat a -> Mod b -> Mod b
plusLeq SZ m = m
plusLeq (SS b) m = plusLeq b (succMod m)

timesLeq :: (SingI b, Num (Mod b)) => SNat a -> Mod b -> Mod b
timesLeq SZ (ModS b) = ModS $ ZeroLeq $ boundOf b
timesLeq (SS b) m = m + timesLeq b m

instance SingI m => Num (Mod ('S m)) where
  ModS a + b = plusLeq (toNat a) b
  ModS a * b = timesLeq (toNat a) b
  abs m = m
  signum m = m
  fromInteger n
    | n > 0 = succMod (fromInteger $ n - 1)
    | otherwise = ModS (ZeroLeq (sing :: SNat m))
  _ - _ = error "unimplemented"

succLeq :: Leq n m -> Leq n ('S m)
succLeq (ZeroLeq n) = ZeroLeq (SS n)
succLeq (SuccLeqSucc l) = SuccLeqSucc $ succLeq l

incMod :: Mod m -> Mod ('S m)
incMod (ModS l) = ModS $ succLeq l

lrefl :: SNat n -> Leq n n
lrefl SZ = ZeroLeq SZ
lrefl (SS n) = SuccLeqSucc $ lrefl n

enumerateM :: SNat n -> [Mod n]
enumerateM SZ = []
enumerateM (SS n) = ModS (lrefl n) : map incMod (enumerateM n)

data IsSing n where
  IsSing :: SingI n => Proxy n -> IsSing n

vec :: forall n a b. SNat n -> (Mod n -> a -> b) -> a -> Vec n b
vec n f x = Vec $ mapLL [] $ enumerateM n
  where
    mapLL :: [b] -> [Mod n] -> [b]
    mapLL acc [] = acc
    mapLL acc (l : r) = mapLL (f l x : acc) r

len :: forall n a. SingI n => Vec n a -> SNat n
len _ = sing

toNat :: Leq n m -> SNat n
toNat (ZeroLeq{}) = SZ
toNat (SuccLeqSucc l) = SS $ toNat l

proj :: forall m a. SingI m => Mod m -> Vec m a -> a
proj (ModS n) (Vec l) = l !! sNatToInt (toNat n)

isSingLe :: Leq n m -> IsSing n
isSingLe (ZeroLeq _) = IsSing Proxy
isSingLe (SuccLeqSucc l) = case isSingLe l of
                   IsSing Proxy -> IsSing Proxy

class Arrow t => ArrowVector t where

  vecDict :: (C t a, C t n) => CDict t (Vec n a)
  natDict :: SingI n => SNat n -> CDict t n

  vproj :: forall a m. (C t a, SingI m) => Mod m -> t (Vec m a) a
  vproj i@(ModS l)
    = case natDict sing :: (CDict t m) of
        CDict ->
          case vecDict :: CDict t (Vec m a) of
            CDict ->
              case isSingLe l of
                IsSing Proxy -> arr "proj" (proj i)

  vmap :: forall a b n. (C t a, C t b, C t n, C t (SNat n), SingI n)
       => t a b -> t (Vec n a) (Vec n b)
  vmap f = case vecDict :: CDict t (Vec n a) of
             CDict -> vgen i fun
    where
      i :: SNat n
      i = sing :: SNat n
      fun :: SingI n => Mod n -> t (Vec n a) b
      fun j =
        case natDict sing :: CDict t n of
          CDict ->
            case vecDict :: CDict t (Vec n a) of
              CDict -> f . vproj j

  vgen :: forall a b n. (C t a, C t b)
       => SNat n -> (Mod n -> t a b) -> t a (Vec n b)
--   vgen n f =
--     case pairDict :: CDict t (Word32, a) of
--       CDict ->
--         case vecDict :: CDict t (Vec n (Word32, a)) of
--           CDict ->
--             case arrDict :: CDict t (t a b) of
--               CDict ->
--                 case pairDict :: CDict t (t a b, a) of
--                   CDict ->
--                     case (vecDict :: CDict t (Vec n (t a b, a))
--                          , vecDict :: CDict t (Vec n b)) of
--                       (CDict, CDict) -> repl >>> vmap app
--     where
--       repl :: (C t a, C t (Vec n (Word32, a))
--              , C t (Vec n (t a b, a)) )
--            => t a (Vec n (t a b, a))
--       repl = arr "(repl)" $ \x -> Vec [(f i, x) | i <- [0..sNatToInt n-1]]

instance ArrowVector (->) where
  vecDict = CDict
  natDict SZ = CDict
  natDict (SS _) = CDict
  vproj = proj
  vmap f (Vec xs) = Vec $ map f xs
  vgen = vec
