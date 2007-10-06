module Data.Probability.Rational (
    module Data.Probability,
    Prob()
  ) where

import Data.Monoid
import Data.Probability

-- | An implementation of 'Data.Probability.Probability' using rational
-- numbers.
newtype Prob = Prob Rational
  deriving (Eq)

instance Probability Prob where
  prob = Prob
  fromProb (Prob p) = p
  pnot (Prob p) = Prob (1-p)
  padd (Prob p1) (Prob p2) = Prob (p1 + p2)
  pmul (Prob p1) (Prob p2) = Prob (p1 * p2)

instance Monoid Prob where
  mempty = prob 1
  mappend = pmul

instance Show Prob where
  show (Prob p) = show p
