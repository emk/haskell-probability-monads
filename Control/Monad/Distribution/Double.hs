module Control.Monad.Distribution.Double (
    module Control.Monad.Distribution,
    DDist
  ) where

import Control.Monad.Distribution
import Control.Monad.MonoidValue
import Data.Probability.Double

-- | A discrete, finite probability distribution implemented using rational
-- numbers.
type DDist = MVT Prob []
