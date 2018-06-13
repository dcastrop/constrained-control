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

class Category t => Const a t where
  const :: forall b. (C t a, C t b) => a -> t b a

type PairC t a b = (Category t, C t a, C t b, C t (a,b), C t (b, a))

class Category t => Arrow t where

  arr :: (C t b, C t c) => String -> (b -> c) -> t b c

  fst :: forall a b. PairC t a b => t (a,b) a
  fst = arr "fst" Prelude.fst

  snd :: forall a b. PairC t a b => t (a,b) b
  snd = arr "snd" Prelude.snd

  first :: (PairC t a c, PairC t b c) => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: (PairC t c a, PairC t c b) => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: forall a b a' b'. (PairC t a b, PairC t a' b', PairC t a' b, PairC t b a')
        => t a a' -> t b b' -> t (a,b) (a',b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: forall a b c. (PairC t a a, PairC t a b, PairC t b c) => t a b -> t a c -> t a (b,c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g

instance Arrow (->) where

  arr _ f = f

  f *** g = \(a, b) -> (f a, g b)

instance Const a (->) where
  const = Prelude.const

type SumC t a b = (Category t, C t a, C t b, C t (Either a b), C t (Either b a))

class Arrow a => ArrowChoice a where

  inl :: forall b c. SumC a b c => a b (Either b c)
  inl = arr "inl" Left

  inr :: forall b c. SumC a b c => a c (Either b c)
  inr = arr "inr" Right

  left :: forall b c d. (SumC a b d, SumC a c d)
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: forall b c d. (SumC a d b, SumC a d c)
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: forall b b' c c'. (SumC a b b', SumC a c c', SumC a c b')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: forall b c d. (SumC a b c, SumC a d d, SumC a d c)
          => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance ArrowChoice (->) where

  inl = Left
  inr = Right

  f +++ g = \case
    Left x -> Left (f x)
    Right x -> Right (g x)

class Arrow t => ArrowApply t where
  app :: forall a b. (PairC t (t a b) a, C t b) => t (t a b, a) b

instance ArrowApply (->) where
  app (f, x) = f x

uncurry :: forall t a b c. (ArrowApply t, C t c, PairC t a b, PairC t b (t b c))
        => (a -> t b c) -> t (a, b) c
uncurry f = app .  (arr "_" f *** id)

curry :: forall t a b c. (Arrow t, Const a t, C t c, PairC t b b, PairC t a b)
        => t (a, b) c -> a -> (t b c)
curry f x = f . (const x &&& id)

unlift :: forall t a b. ( Const a t, C t a, C t b, C t () ) => t a b -> a -> t () b
unlift f x = f . const x
