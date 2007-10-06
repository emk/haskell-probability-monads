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
    Dist, weighted, uniform,
    module Control.Monad.Random,
  ) where

import Control.Monad.Random

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

-- Make all the standard instances of MonadRandom into probability
-- distributions.
instance (Monad m, RandomGen g) => Dist (RandT g m) where 
  weighted = fromList
