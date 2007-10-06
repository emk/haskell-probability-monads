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
    BRand, sample, sampleIO,
    -- * Discrete, finite distributions
    -- $DDist
    bayes
  ) where

import Control.Monad
import Control.Monad.Maybe
import Control.Monad.MonoidValue
import Control.Monad.Random
import Control.Monad.Trans
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

-- | A distribution which supports 'Dist' and 'Control.Monad.MonadPlus'
-- supports Bayes' rule.  Use 'Control.Monad.guard' to calculate a
-- conditional probability.
class (Dist d, MonadPlus d) => BayesDist d
  -- TODO: Do we want to add an associated type here, pointing to the
  -- underlying distribution type?

-- Applying MaybeT to a distribution gives you another distribution, but
-- with support for Bayes' rule.
instance (Dist d) => Dist (MaybeT d) where
  weighted wvs = lift (weighted wvs)

{- $Rand
-}

-- Make all the standard instances of MonadRandom into probability
-- distributions.
instance (Monad m, RandomGen g) => Dist (RandT g m) where 
  weighted = fromList

-- | 
type BRand g = MaybeT (Rand g)

instance (RandomGen g, Monad m) => BayesDist (MaybeT (RandT g m))

instance (RandomGen g, Monad m) => MonadPlus (MaybeT (RandT g m)) where
  mzero = MaybeT (return Nothing)
  -- TODO: I'm not sure this is particularly sensible or useful.
  d1 `mplus` d2 = MaybeT choose
    where choose = do
            x1 <- runMaybeT d1
            case x1 of
              Nothing -> runMaybeT d2
              Just _  -> return x1

-- | Take @n@ samples from the distribution @r@.
sample :: (MonadRandom m) => m a -> Int -> m [a]
sample r n = sequence (replicate n r)

-- | Take @n@ samples from the distribution @r@ using the IO monad.
sampleIO r n = evalRandIO (sample r n)
sampleIO :: Rand StdGen a -> Int -> IO [a]

{- $DDist
-}

instance (Probability p) => Dist (MVT p []) where
  weighted wvs = MVT (map toMV wvs)
    where toMV (v, w) = MV (prob (w / total)) v 
          total = sum (map snd wvs)

instance (Probability p) => BayesDist (MaybeT (MVT p [])) where

instance (Probability p) => MonadPlus (MaybeT (MVT p [])) where
  mzero = MaybeT (return Nothing)
  -- TODO: I'm not sure this is particularly sensible or useful.
  d1 `mplus` d2
     | isNothing (bayes d1)  = d2
     | otherwise             = d1

catMaybes' :: (Monoid w) => [MV w (Maybe a)] -> [MV w a]
catMaybes' = map (liftM fromJust) . filter (isJust . mvValue)

-- | Apply Bayes' rule, discarding impossible outcomes and normalizing the
-- probabilities that remain.
--
-- TODO: It's entirely possible that this method should be moved to a type
-- class.
bayes :: (Probability p) =>
         MaybeT (MVT p []) a -> Maybe ((MVT p []) a)
bayes bfd
    | total == prob 0 = Nothing
    | otherwise       = Just (weighted (map unpack events))
  where
    events = catMaybes' (runMVT (runMaybeT bfd))
    total  = foldl' padd (prob 0) (map mvMonoid events)
    unpack (MV p v) = (v, fromProb p)
