{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Control.Constrained.Category
  ( Category (..)
  , NoConstraint
  , CDict (..)
  , (>>>)
  , (<<<)
  ) where

import qualified Prelude
import Prelude hiding ( (.), id )

import Data.Constraint


infixr 9 .
infixr 1 >>>, <<<

class Category (t :: * -> * -> *) where
  type C t :: k -> Constraint
  type C t = NoConstraint
  id :: forall a. C t a => t a a
  (.) :: forall a b c. (C t a, C t b, C t c) => t b c -> t a b -> t a c

class NoConstraint a where
instance NoConstraint a where

data CDict t a where
  CDict :: (Category t, C t a) => CDict t a

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)

-- | Right-to-left composition
(<<<) :: (Category cat, C cat a, C cat b, C cat c)
      => cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (Category cat, C cat a, C cat b, C cat c)
      => cat a b -> cat b c -> cat a c
f >>> g = g . f
