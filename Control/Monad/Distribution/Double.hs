module Control.Monad.Distribution.Double (
    module Control.Monad.Distribution,
    DDist
  ) where

import Control.Monad.Distribution
import Control.Monad.Maybe
import Control.Monad.MonoidValue
import Data.Probability.Double

-- | A discrete, finite probability distribution implemented using rational
-- numbers.
type DDist = MVT Prob []

-- | A version of 'BDDist' with support for Bayes' theorem.
type BDDist = MaybeT DDist
