{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental

Common interface for probability distribution monads.  Heavily inspired by
Martin Erwig's and Steve Kollmansberger's /Probabilistic Functional
Programming/, which can be found at
<http://web.engr.oregonstate.edu/~erwig/pfp/>.

For background, see Michele Giry, /A Categorical Approach to Probability
Theory/.

-}

module Control.Monad.Distribution (
    -- * Common interface
    -- $Interface
    Dist, weighted, uniform,
    -- * Random sampling functions
    -- $Rand
    module Control.Monad.Random,
    BRand,
    -- * Discrete, finite distributions
    -- $DDist
    bayes
  ) where

import Control.Monad
import Control.Monad.Maybe
import Control.Monad.MonoidValue
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Probability

{- $Interface
-}

-- | Represents a probability distribution.
class (Functor d, Monad d) => Dist d where
  -- | Creates a new distribution from a weighted list of values.  The
  -- individual weights must be non-negative, and they must sum to a
  -- positive number.
  weighted :: [(a, Rational)] -> d a
  -- TODO: What order do we want weighted's arguments in?

-- | Creates a new distribution from a list of values, weighting it evenly.
uniform :: Dist d => [a] -> d a
uniform = weighted . map (\x -> (x, 1))

{- $Rand
-}

-- Make all the standard instances of MonadRandom into probability
-- distributions.
instance (Monad m, RandomGen g) => Dist (RandT g m) where 
  weighted = fromList

-- | 
type BRand g = MaybeT (Rand g)

instance (RandomGen g) => MonadPlus (BRand g) where
  mzero = MaybeT (return Nothing)
  -- TODO: I'm not sure this is particularly sensible or useful.
  d1 `mplus` d2 = MaybeT choose
    where choose = do
            x1 <- runMaybeT d1
            case x1 of
              Nothing -> runMaybeT d2
              Just _  -> return x1

{- $DDist
-}

instance (Probability p) => Dist (MVT p []) where
  weighted wvs = MVT (map toMV wvs)
    where toMV (v, w) = MV (prob (w / total)) v 
          total = sum (map snd wvs)

catMaybes' :: (Monoid w) => [MV w (Maybe a)] -> [MV w a]
catMaybes' = map (liftM fromJust) . filter (isJust . mvValue)

bayes :: (Probability p) =>
         MaybeT (MVT p []) a -> Maybe ((MVT p []) a)
bayes bfd
    | total == pzero = Nothing
    | otherwise      = Just (weighted (map unpack events))
  where
    events = catMaybes' (runMVT (runMaybeT bfd))
    total  = foldl' padd pzero (map mvMonoid events)
    unpack (MV p v) = (v, fromProb p)
