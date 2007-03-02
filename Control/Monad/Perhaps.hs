{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental

Generalization of 'Maybe' to work with probability values between 0 and 1.

-}

module Control.Monad.Perhaps (
    -- * Perhaps
    Perhaps(..), never, always, possible, impossible, perhaps,
    -- * PerhapsT
    PerhapsT(..)
  ) where

import Data.Prob
import Control.Monad
import Control.Monad.Trans

data Perhaps a = Perhaps { perhapsValue :: a, perhapsProb :: Prob }

instance Show a => Show (Perhaps a) where
  show (Perhaps _ 0) = "never"
  show (Perhaps x p) = "Perhaps " ++ show x ++ " " ++ show p

never :: Perhaps a
never = Perhaps undefined 0

always :: a -> Perhaps a
always x = Perhaps x         1

impossible :: Perhaps a -> Bool
impossible p = perhapsProb p == 0

possible :: Perhaps a -> Bool
possible = not . impossible

perhaps :: b -> (a -> b) -> Perhaps a -> b
perhaps defaultValue f ph | impossible ph = defaultValue
                          | otherwise     = f (perhapsValue ph)

instance Functor Perhaps where
  fmap f (Perhaps x p) = Perhaps (f x) p

instance Monad Perhaps where
  return x = Perhaps x 1
  -- Note that if (*) were non-strict in its first argument, we wouldn't need
  -- to handle 'never' separately.
  ph >>= f | impossible ph = never
           | otherwise     = Perhaps x (p1 * p2)
    where (Perhaps (Perhaps x p1) p2) = fmap f ph

newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

instance MonadTrans PerhapsT where
  -- TODO: Assigns every event a probability of 1.  Obviously, this doesn't
  -- always make sense to call.
  lift x = PerhapsT (liftM return x)

instance Functor m => Functor (PerhapsT m) where
  fmap f = PerhapsT . fmap (fmap f) . runPerhapsT

instance Monad m => Monad (PerhapsT m) where
  return = lift . return
  m >>= f = PerhapsT bound
    where bound = do
            ph <- runPerhapsT m
            case ph of
              (Perhaps x1 p1)  | p1 == 0    -> return never
                               | otherwise  -> do
                (Perhaps x2 p2) <- runPerhapsT (f x1)
                return (Perhaps x2 (p1 * p2))
