{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Constrained.Arrow
  ( Arrow (..)
  , Const (..)
  , ArrowChoice (..)
  , ArrowApply (..)
  , PairC
  , SumC
  , curry
  , uncurry
  , unlift
  ) where

import qualified Prelude
import Prelude hiding ( (.), id, fst, snd, const, curry, uncurry)

import Control.Constrained.Category

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

class Category cc t => Const cc a t where
  const :: forall b. (cc a, cc b) => a -> t b a

type PairC cc t a b = (Category cc t, cc a, cc b, cc (a,b), cc (b, a))

class Category cc t => Arrow cc t where

  arr :: (cc b, cc c) => String -> (b -> c) -> t b c

  fst :: forall a b. PairC cc t a b => t (a,b) a
  fst = arr "fst" Prelude.fst

  snd :: forall a b. PairC cc t a b => t (a,b) b
  snd = arr "snd" Prelude.snd

  first :: (PairC cc t a c, PairC cc t b c) => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: (PairC cc t c a, PairC cc t c b) => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: forall a b a' b'. (PairC cc t a b, PairC cc t a' b', PairC cc t a' b, PairC cc t b a')
        => t a a' -> t b b' -> t (a,b) (a',b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: forall a b c. (PairC cc t a a, PairC cc t a b, PairC cc t b c) => t a b -> t a c -> t a (b,c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g

instance Arrow NoConstraint (->) where

  arr _ f = f

  f *** g = \(a, b) -> (f a, g b)

instance Const NoConstraint a (->) where
  const = Prelude.const

type SumC cc t a b = (Category cc t, cc a, cc b, cc (Either a b), cc (Either b a))

class Arrow cc a => ArrowChoice cc a where

  inl :: forall b c. SumC cc a b c => a b (Either b c)
  inl = arr "inl" Left

  inr :: forall b c. SumC cc a b c => a c (Either b c)
  inr = arr "inr" Right

  left :: forall b c d. (SumC cc a b d, SumC cc a c d)
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: forall b c d. (SumC cc a d b, SumC cc a d c)
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: forall b b' c c'. (SumC cc a b b', SumC cc a c c', SumC cc a c b')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: forall b c d. (SumC cc a b c, SumC cc a d d, SumC cc a d c)
          => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance ArrowChoice NoConstraint (->) where

  inl = Left
  inr = Right

  f +++ g = \case
    Left x -> Left (f x)
    Right x -> Right (g x)

class Arrow cc t => ArrowApply cc t where
  app :: forall a b. (PairC cc t (t a b) a, cc b) => t (t a b, a) b

instance ArrowApply NoConstraint (->) where
  app (f, x) = f x

uncurry :: forall t cc a b c. (ArrowApply cc t, cc c, PairC cc t a b, PairC cc t b (t b c))
        => (a -> t b c) -> t (a, b) c
uncurry f = app .  (arr "_" f *** id)

curry :: forall t cc a b c. (Arrow cc t, Const cc a t, cc c, PairC cc t b b, PairC cc t a b)
        => t (a, b) c -> a -> (t b c)
curry f x = f . (const x &&& id)

unlift :: forall t cc a b. (Const cc a t, cc a, cc b, cc ()) => t a b -> a -> t () b
unlift f x = f . const x
