{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental

This module is a wrapper around @Control.Monad.Distribution.Base@.  It
provides definitions of 'DDist', 'ddist', 'BDDist' and 'bddist' based on
double-precion floating point numbers.

For the main API, see @Control.Monad.Distribution.Base@.  For alternative
versions of 'DDist', etc., based on exact rational numbers, see
@Control.Monad.Distribution.Rational@.

-}

module Control.Monad.Distribution (
    module Control.Monad.Distribution.Base,
    DDist, ddist, BDDist, bddist
  ) where

import Control.Monad.Distribution.Base
import Control.Monad.Maybe
import Control.Monad.MonoidValue
import Data.Probability

-- | A discrete, finite probability distribution implemented using rational
-- numbers.
type DDist = MVT Prob []

-- | Force a value to be interpreted as having type 'DDist'.
ddist :: DDist a -> DDist a
ddist d = d

-- | A version of 'BDDist' with support for Bayes' theorem.
type BDDist = MaybeT DDist

-- | Force a value to be interpreted as having type 'BDDist', and apply
-- Bayes' rule.  Returns 'Nothing' if no possible combination of events
-- will satisfy the guard conditions specified in 'BDDist'.
bddist :: BDDist a -> Maybe (DDist a)
bddist d = bayes d
