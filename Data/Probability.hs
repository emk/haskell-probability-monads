module Data.Probability (
    module Data.Probability.Base,
    Prob()
  ) where

import Data.Monoid
import Data.Probability.Base

-- | An implementation of 'Data.Probability.Probability' using
-- double-precision floating-point numbers.
newtype Prob = Prob Double
  deriving (Eq)

instance Probability Prob where
  prob = Prob . fromRational
  fromProb (Prob p) = toRational p
  pnot (Prob p) = Prob (1-p)
  padd (Prob p1) (Prob p2) = Prob (p1 + p2)
  pmul (Prob p1) (Prob p2) = Prob (p1 * p2)

instance Monoid Prob where
  mempty = prob 1
  mappend = pmul

instance Show Prob where
  show (Prob p) = show p
