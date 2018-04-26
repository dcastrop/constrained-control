{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
module Control.Constrained.Category
  ( Category (..)
  , (>>>)
  , (<<<)
  ) where

import Prelude hiding ( (.), id )

import qualified Control.Category as Category
import Data.Constraint


infixr 9 .
infixr 1 >>>, <<<

class Category (t :: * -> * -> *) where
  type C t (a :: *) :: Constraint
  id :: forall a. C t a => t a a
  (.) :: forall a b c. (C t a, C t b, C t c) => t b c -> t a b -> t a c

class NoConstraint a where
instance NoConstraint a where

instance Category.Category t => Category t where
  type C t a = NoConstraint a
  id  = Category.id
  (.) = (Category..)

-- | Right-to-left composition
(<<<) :: (Category cat, C cat a, C cat b, C cat c)
      => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (Category cat, C cat a, C cat b, C cat c)
      => cat a b -> cat b c -> cat a c
f >>> g = g . f
