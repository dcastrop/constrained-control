{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Constrained.Arrow
  ( Arrow (..)
  , ArrowChoice (..)
  ) where

import qualified Prelude
import Prelude hiding ( (.), id, fst, snd)

import Control.Constrained.Category
import qualified Control.Arrow as Arrow

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

class Category t => Arrow t where
  arr :: (C t b, C t c) => String -> (b -> c) -> t b c

  fst :: (C t a, C t b)
      => t (a,b) a
  fst = arr "fst" Prelude.fst

  snd :: (C t a, C t b)
      => t (a,b) b
  snd = arr "snd" Prelude.snd

  first :: (C t a, C t b, C t c)
        => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: (C t a, C t b, C t c)
         => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: (C t a, C t a', C t b, C t b')
         => t a a' -> t b b' -> t (a,b) (a',b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: (C t a, C t b, C t c)
        => t a b -> t a c -> t a (b,c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g


instance Arrow.Arrow t => Arrow t where
  arr _ = Arrow.arr
  first = Arrow.first
  second = Arrow.second
  (***) = (Arrow.***)
  (&&&) = (Arrow.&&&)

class Arrow a => ArrowChoice a where

  inl :: (C a b, C a c)
      => a b (Either b c)
  inl = arr "inl" Left

  inr :: (C a b, C a c)
      => a c (Either b c)
  inr = arr "inr" Right

  left :: (C a b, C a c, C a d)
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: (C a b, C a c, C a d)
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: (C a b, C a b', C a c, C a c')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: (C a b, C a d, C a c)
        => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance Arrow.ArrowChoice t => ArrowChoice t where
  left = Arrow.left
  right = Arrow.right
  (+++) = (Arrow.+++)
  (|||) = (Arrow.|||)
