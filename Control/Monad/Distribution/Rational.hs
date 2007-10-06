module Control.Monad.Distribution.Rational (
    module Control.Monad.Distribution,
    DDist
  ) where

import Control.Monad.Distribution
import Control.Monad.MonoidValue
import Data.Probability.Rational

-- | A discrete, finite probability distribution implemented using
-- double-precision floating-point numbers.
type DDist = MVT Prob []
