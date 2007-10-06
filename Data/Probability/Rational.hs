module Data.Probability.Rational (
    module Data.Probability,
    Prob()
  ) where

import Data.Probability

-- | An implementation of 'Data.Probability.Probability' using rational
-- numbers.
newtype Prob = Prob Rational
  deriving (Eq)

instance Probability Prob where
  pzero = Prob 0
  pone = Prob 1
  pnot (Prob p) = Prob (1-p)
  padd (Prob p1) (Prob p2) = Prob (p1 + p2)
  pmul (Prob p1) (Prob p2) = Prob (p1 * p2)

instance Show Prob where
  show (Prob p) = show p
