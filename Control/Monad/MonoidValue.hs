{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE DatatypeContexts #-}

{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental
Portability  : non-portable (multi-parameter type classes, undecidable instances)

This module provides stripped-down versions of
'Control.Monad.Writer.Writer' and 'Control.Monad.Writer.WriterT', minus the
operations 'Control.Monad.Writer.tell', 'Control.Monad.Writer.listen' and
'Control.Monad.Writer.pass'.  It a useful building block for monads
representing probability distributions or quantum states, where the extra
functions provided by 'Control.Monad.Writer.Class.MonadWriter' are
irrelevant or inappropriate.

The 'MV' monad and the 'MVT' monad transformer were proposed by Dan Piponi
as a way of representing M-sets in Haskell.  An /M-set/ is a set with a
monoid action (by analogy to the more common G-sets found in group theory).
Here, 'MV' represents an element in a /free M-set/.  This is essentially a
(monoid,value) pair.

[Computation type:] Computations with an associated monoid action.

[Binding strategy:] The @return@ function lifts a value into the monad by
pairing it with @mempty@.  The @bind@ function uses @mappend@ to implement
the monoid action.

[Useful for:] Building probability distribution monads.

-}

module Control.Monad.MonoidValue (
    module Data.Monoid,
    MV(MV), mvMonoid, mvValue, MVT(MVT), runMVT
  ) where

import Control.Monad.Trans
import Data.Monoid

-- | A value annotated with a monoid.  Represents an element in a free
-- M-set.
data (Monoid w) => MV w a =
  MV { mvMonoid :: w, mvValue :: a }

instance (Monoid w, Show w, Show a) => Show (MV w a) where
  show (MV w a) = "MV " ++ show w ++ " " ++ show a

-- We build our functor and monad instances from 'mapMV' and 'joinMV' for
-- simplicity.
mapMV :: (Monoid w) => (a -> b) -> MV w a -> MV w b
mapMV f (MV w v) = MV w (f v)

joinMV :: (Monoid w) => MV w (MV w a) -> MV w a
joinMV (MV w1 (MV w2 v)) = MV (w1 `mappend` w2) v

instance (Monoid w) => Functor (MV w) where
  fmap = mapMV

instance (Monoid w) => Monad (MV w) where
  return v = MV mempty v
  mv >>= f = joinMV (mapMV f mv)

-- | Transforms a monad @m@ to associate a monoid value with the
-- computation.
newtype (Monoid w, Monad m) => MVT w m a =
  MVT { runMVT :: m (MV w a) }

instance (Monoid w) => MonadTrans (MVT w) where
  lift mv = MVT (do v <- mv
                    return (MV mempty v))

instance (Monoid w, Monad m) => Functor (MVT w m) where
  fmap f ma = MVT mapped
    where mapped = do
            (MV w v) <- runMVT ma
            return (MV w (f v))

instance (Monoid w, Monad m) => Monad (MVT w m) where
  return   = lift . return
  ma >>= f = MVT bound
    where  bound = do
             (MV w1 v1) <- runMVT ma
             (MV w2 v2) <- runMVT (f v1)
             return (MV (w1 `mappend` w2) v2)

