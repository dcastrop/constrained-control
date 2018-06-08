{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Constrained.Arrow
  ( Arrow (..)
  , Const (..)
  , ArrowChoice (..)
  , ArrowApply (..)
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

class Category t => Arrow t where

  pairDict :: (C t a, C t b) => CDict t (a,b)

  arr :: (C t b, C t c) => String -> (b -> c) -> t b c

  fst :: forall a b. (C t a, C t b) => t (a,b) a
  fst = case pairDict :: CDict t (a, b) of
          CDict -> arr "fst" Prelude.fst

  snd :: forall a b. (C t b, C t a) => t (a,b) b
  snd = case pairDict :: CDict t (a, b) of
          CDict -> arr "snd" Prelude.snd

  first :: ( C t a, C t c, C t b )
        => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: ( C t c, C t a, C t b )
         => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: forall a b a' b'. ( C t a, C t b, C t a', C t b' )
         => t a a' -> t b b' -> t (a,b) (a',b')
  f *** g = case ( pairDict :: CDict t (a , b)
                 , pairDict :: CDict t (a', b)
                 , pairDict :: CDict t (a', b')
                 , pairDict :: CDict t (b, a')
                 , pairDict :: CDict t (b', a')
                 ) of
              (CDict, CDict, CDict, CDict, CDict) ->
                first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: forall a b c. ( C t a, C t b, C t c ) => t a b -> t a c -> t a (b,c)
  f &&& g = case ( pairDict :: CDict t (a, a)
                 , pairDict :: CDict t (b, c) ) of
              (CDict, CDict) -> arr "dup" (\b -> (b,b)) >>> f *** g

instance Arrow (->) where

  pairDict = CDict

  arr _ f = f

  f *** g = \(a, b) -> (f a, g b)

instance Const a (->) where
  const = Prelude.const


class Arrow a => ArrowChoice a where

  eitherDict :: (C a x, C a y) => CDict a (Either x y)

  inl :: forall b c. ( C a b, C a c ) => a b (Either b c)
  inl = case eitherDict :: CDict a (Either b c) of
          CDict -> arr "inl" Left

  inr :: forall b c. ( C a c, C a b ) => a c (Either b c)
  inr = case eitherDict :: CDict a (Either b c) of
          CDict -> arr "inr" Right

  left :: forall b c d. ( C a b, C a d, C a c )
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: forall b c d. ( C a d, C a b , C a c )
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: forall b b' c c'. ( C a b, C a b', C a c, C a c')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g =
    case ( eitherDict :: CDict a (Either b b')
         , eitherDict :: CDict a (Either c b')
         , eitherDict :: CDict a (Either c c')
         , eitherDict :: CDict a (Either c' c)
         , eitherDict :: CDict a (Either b' b)
         , eitherDict :: CDict a (Either b' c)
         ) of
        (CDict, CDict, CDict, CDict, CDict, CDict) ->
          left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: forall b c d. ( C a b, C a c, C a d )
          => a b d -> a c d -> a (Either b c) d
  f ||| g = case ( eitherDict :: CDict a (Either b c)
                 , eitherDict :: CDict a (Either d d) ) of
              (CDict, CDict) -> f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

instance ArrowChoice (->) where

  eitherDict = CDict

  inl = Left
  inr = Right

  f +++ g = \case
    Left x -> Left (f x)
    Right x -> Right (g x)

class Arrow t => ArrowApply t where
  arrDict :: (C t a, C t b) => CDict t (t a b)
  app :: forall a b. ( C t a, C t b ) => t (t a b, a) b

instance ArrowApply (->) where
  arrDict = CDict
  app (f, x) = f x

uncurry :: forall t a b c. ( ArrowApply t , C t a, C t b, C t c
                     , C t (a, b), C t (t b c, b), C t (t b c) )
        => (a -> t b c) -> t (a, b) c
uncurry f = app .  (arr "_" f *** id)

curry :: forall t a b c. ( Const a t, ArrowApply t, C t b, C t (a, b), C t c ,C t a )
        => t (a, b) c -> a -> (t b c)
curry f x = f . (const x &&& id)

unlift :: forall t a b. ( Const a t, C t a, C t b, C t () ) => t a b -> a -> t () b
unlift f x = f . const x
