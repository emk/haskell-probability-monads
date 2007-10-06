module Control.Monad.Distribution.Rational (
    module Control.Monad.Distribution,
    DDist, BDDist
  ) where

import Control.Monad.Distribution
import Control.Monad.Maybe
import Control.Monad.MonoidValue
import Data.Probability.Rational

-- | A discrete, finite probability distribution implemented using
-- double-precision floating-point numbers.
type DDist = MVT Prob []

-- | A version of 'BDDist' with support for Bayes' theorem.
type BDDist = MaybeT DDist
