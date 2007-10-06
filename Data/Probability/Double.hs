module Data.Probability.Double (
    module Data.Probability,
    Prob()
  ) where

import Data.Monoid
import Data.Probability

-- | An implementation of 'Data.Probability.Probability' using
-- double-precision floating-point numbers.
newtype Prob = Prob Double
  deriving (Eq)

instance Monoid Prob where
  mempty = Prob 1
  mappend (Prob p1) (Prob p2) = Prob (p1 * p2)

instance Probability Prob where
  pzero = Prob 0
  pnot (Prob p) = Prob (1-p)
  padd (Prob p1) (Prob p2) = Prob (p1 + p2)

instance Show Prob where
  show (Prob p) = show p
