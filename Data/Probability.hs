{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental
Portability  : non-portable (newtype deriving)

Support for probability values.
-}

module Data.Probability (Probability, pzero, pone, pnot, padd, pmul) where

import Data.Monoid

-- | The probability of an event occuring.  We provide this as a type
-- class, allowing users of this library to choose among various
-- representations of probability.
class (Eq p, Monoid p) => Probability p where
  -- | The probability of an impossible event.
  pzero :: p
  -- | The probability of an event which always occurs.
  pone :: p
  pone = mempty
  -- | Given the probability of an event occuring, calculate the
  -- probability of the event /not/ occuring.
  pnot :: p -> p
  -- | Given the probabilities of two disjoint events, calculate the
  -- probability of either event occuring.
  padd :: p -> p -> p
  -- | Given the probabilities of two indepedent events, calculate the
  -- probability of both events occuring.
  pmul :: p -> p -> p
  pmul = mappend

{-
We probably want something like this somewhere...

-- | Convert a probability to a floating-point number.
doubleFromProb :: Prob -> Double
doubleFromProb (Prob d) = d
-}
