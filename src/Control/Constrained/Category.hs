{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Constrained.Category
  ( Category (..)
  , NoConstraint
  , (>>>)
  , (<<<)
  ) where


import qualified Prelude
import Prelude hiding ( (.), id )

import Data.Constraint

infixr 9 .
infixr 1 >>>, <<<

class Category (cc :: k -> Constraint) (t :: k -> k -> *) | t -> cc where
  id :: forall a. cc a => t a a
  (.) :: forall a b c. (cc a, cc b, cc c) => t b c -> t a b -> t a c

class NoConstraint a where
instance NoConstraint a where

instance Category NoConstraint (->) where
  id = Prelude.id
  (.) = (Prelude..)

-- | Right-to-left composition
(<<<) :: (Category cc cat, cc a, cc b, cc c)
      => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (Category cc cat, cc a, cc b, cc c)
      => cat a b -> cat b c -> cat a c
f >>> g = g . f
