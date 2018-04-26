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

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

class Category t => Arrow t where

  arr :: (C t b, C t c) => String -> (b -> c) -> t b c

  fst :: (C t a, C t (a,b))
      => t (a,b) a
  fst = arr "fst" Prelude.fst

  snd :: (C t b, C t (a,b))
      => t (a,b) b
  snd = arr "snd" Prelude.snd

  first :: ( C t c, C t b, C t (a,c) , C t (b,c), C t (c, b) )
        => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: ( C t c, C t a, C t b
           , C t (a,c) , C t (b,c), C t (c, a), C t (c, b) )
         => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: ( C t b, C t a', C t b', C t (a, b), C t (a', b'), C t (a', b)
          , C t (b, a') , C t (b', a') )
         => t a a' -> t b b' -> t (a,b) (a',b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: ( C t a, C t b, C t c, C t (b, a), C t (a, b), C t (c, b)
          , C t (a, a), C t (b, c) )
        => t a b -> t a c -> t a (b,c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g


class Arrow a => ArrowChoice a where

  inl :: ( C a b, C a (Either b c) )
      => a b (Either b c)
  inl = arr "inl" Left

  inr :: ( C a c, C a (Either b c) )
      => a c (Either b c)
  inr = arr "inr" Right

  left :: ( C a d, C a c,  C a (Either d b), C a (Either c d)
         , C a (Either d b), C a (Either b d), C a (Either d c) )
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: ( C a d, C a b
          , C a c
          , C a (Either d b), C a (Either d c)
          , C a (Either b d), C a (Either c d) )
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: ( C a b', C a c, C a c'
          , C a (Either b b')
          , C a (Either c b')
          , C a (Either c c')
          , C a (Either c' c)
          , C a (Either b' b)
          , C a (Either b' c)
          )
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: ( C a c, C a d
          , C a (Either d c)
          , C a (Either d d)
          , C a (Either b c)
          , C a (Either c b)
          , C a (Either c d) )
          => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y
