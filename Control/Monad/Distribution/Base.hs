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

module Control.Monad.Distribution.Base (
    -- * Common interface
    -- $Interface
    Dist, weighted, uniform,
    -- * Bayes' rule
    -- $Bayes
    MonadPlus, mzero, mplus, guard, -- Re-exported from Control.Monad.
    -- * Random sampling functions
    -- $Rand
    module Control.Monad.Random,
    sample, sampleIO,
    BRand, sampleBayes, sampleBayesIO,
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

Common interfaces to probability monads.  For example, if we assume that a
family has two children, each a boy or a girl, we can build a probability
distribution representing all such families.

>{-# LANGUAGE NoMonomorphismRestriction #-}
>
>import Control.Monad.Distribution
>
>data Child = Girl | Boy
>  deriving (Show, Eq, Ord)
>
>child = uniform [Girl, Boy]
>
>family = do
>  child1 <- child
>  child2 <- child
>  return [child1, child2]

The use of @NoMonomorphismRestriction@ is optional.  It eliminates the need
for type declarations on @child@ and @family@:

>child :: (Dist d) => d Child
>child = uniform [Girl, Boy]
>
>family :: (Dist d) => d [Child]
>family = ...

Unfortunately, using @NoMonomorphismRestriction@ may hide potential
performance issues.  In either of the above examples, Haskell compilers may
recompute @child@ from scratch each time it is called, because the actual
type of the distribution @d@ is unknown.  Normally, Haskell requires an
explicit type declaration in this case, in hope that you will notice the
potential performance issue.  By enabling @NoMonomorphismRestriction@, you
indicate that you intended the code to work this way, and don't wish to use
type declarations on every definition.

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

{- $Bayes

Using 'Control.Monad.guard', it's possible to calculate conditional
probabilities using Bayes' rule.  In the example below, we choose to
@Control.Monad.Distribution.Rational@, which calculates probabilities using
exact rational numbers.  This is useful for small, interactive programs
where you want answers like 1/3 and 2/3 instead of 0.3333333 and 0.6666666.

>{-# LANGUAGE NoMonomorphismRestriction #-}
>
>import Control.Monad
>import Control.Monad.Distribution.Rational
>import Data.List
>
>data Coin = Heads | Tails
>  deriving (Eq, Ord, Show)
>
>toss = uniform [Heads, Tails]
>
>tosses n = sequence (replicate n toss)
>
>tossesWithAtLeastOneHead n = do
>  result <- tosses n
>  guard (Heads `elem` result)
>  return result

In this example, we use 'Control.Monad.guard' to discard possible outcomes
where no coin comes up heads.

-}


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

Support for probability distributions represented by sampling functions.
This API is heavily inspired by Sungwoo Park and colleagues'
$\lambda_{\bigcirc}$ caculus <http://citeseer.ist.psu.edu/752237.html>.

Two sampling-function monads are available: 'Control.Monad.Random.Rand' and
'BRand'.  The former provides ordinary sampling functions, and the latter
supports Bayesian reasoning.

It's possible run code in the 'Control.Monad.Random.Rand' monad using
either 'sample' or 'sampleIO'.

>sampleIO family 3
>-- [[Boy,Girl],[Boy,Girl],[Girl,Girl]]

If the probability distribution uses 'Control.Monad.guard', you can run it
using 'sampleBayesIO'.  Note that one of the outcomes below was discarded,
leaving 3 outcomes instead of the expected 4:

>sampleBayesIO (tossesWithAtLeastOneHead 2) 4
>-- [[Tails,Heads],[Heads,Heads],[Tails,Heads]]

-}

-- Make all the standard instances of MonadRandom into probability
-- distributions.
instance (RandomGen g) => Dist (Rand g) where
  weighted = fromList
instance (Monad m, RandomGen g) => Dist (RandT g m) where 
  weighted = fromList

-- | Take @n@ samples from the distribution @r@.
sample :: (MonadRandom m) => m a -> Int -> m [a]
sample d n = sequence (replicate n d)

-- | Take @n@ samples from the distribution @r@ using the IO monad.
sampleIO :: Rand StdGen a -> Int -> IO [a]
sampleIO d n = evalRandIO (sample d n)

-- | A random distribution where some samples may be discarded.
type BRand g = MaybeT (Rand g)

instance (RandomGen g) => BayesDist (MaybeT (Rand g))
instance (RandomGen g, Monad m) => BayesDist (MaybeT (RandT g m))

instance (RandomGen g) => MonadPlus (MaybeT (Rand g)) where
  mzero = randMZero
  mplus = randMPlus

instance (RandomGen g, Monad m) => MonadPlus (MaybeT (RandT g m)) where
  mzero = randMZero
  mplus = randMPlus

randMZero :: (MonadRandom m) => (MaybeT m a)
randMZero = MaybeT (return Nothing)

-- TODO: I'm not sure this is particularly sensible or useful.
randMPlus :: (MonadRandom m) => (MaybeT m a) -> (MaybeT m a) -> (MaybeT m a)
randMPlus d1 d2 = MaybeT choose
  where choose = do
          x1 <- runMaybeT d1
          case x1 of
            Nothing -> runMaybeT d2
            Just _  -> return x1


-- | Take @n@ samples from the distribution @r@, and eliminate any samples
-- which fail a 'Control.Monad.guard' condition.
sampleBayes :: (MonadRandom m) => MaybeT m a -> Int -> m [a]
sampleBayes d n = liftM catMaybes (sample (runMaybeT d) n)

-- | Take @n@ samples from the distribution @r@ using the IO monad, and
-- eliminate any samples which fail a 'Control.Monad.guard' condition.
sampleBayesIO :: BRand StdGen a -> Int -> IO [a]
sampleBayesIO d n = evalRandIO (sampleBayes d n)

{- $DDist

Using the 'Control.Monad.Distribution.DDist' and
'Control.Monad.Distribution.BDDist' monads, you can compute exact
distributions. For example:

>ddist family
>-- [MV 0.25 [Girl,Girl],
>--  MV 0.25 [Girl,Boy],
>--  MV 0.25 [Boy,Girl],
>--  MV 0.25 [Boy,Boy]]

If the probability distribution uses 'Control.Monad.guard', you can run it
using 'Control.Monad.Distribution.bddist'.

>bddist (tossesWithAtLeastOneHead 2)
>-- Just [MV 1%3 [Heads,Heads],
>--       MV 1%3 [Heads,Tails],
>--       MV 1%3 [Tails,Heads]]

Note that we see rational numbers in this second example, because we used
Control.Monad.Distribution.Rational above.

-}

instance (Probability p) => Dist (MVT p []) where
  weighted wvs = MVT (map toMV wvs)
    where toMV (v, w) = MV (prob (w / total)) v 
          total = sum (map snd wvs)

instance (Show a, Ord a, Show p, Probability p) => Show (MVT p [] a) where
  show = show . simplify . runMVT

simplify :: (Probability p, Ord a) => [MV p a] -> [MV p a]
simplify = map (foldr1 merge) . groupEvents . sortEvents
  where sortEvents = sortBy (liftOp compare)
        groupEvents = groupBy (liftOp (==))
        liftOp op  (MV _   v1) (MV _   v2)  = op v1 v2
        merge      (MV w1  v1) (MV w2  _)   = MV (w1 `padd` w2) v1

instance (Probability p) => BayesDist (MaybeT (MVT p []))

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
