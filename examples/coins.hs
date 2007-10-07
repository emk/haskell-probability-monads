{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Control.Monad.Distribution.Rational
-- or import Control.Monad.Distribution.Rational if you want exact answers
import Data.List

data Coin = Heads | Tails
  deriving (Eq, Ord, Show)

toss = uniform [Heads, Tails]

tosses n = sequence (replicate n toss)

unorderedTosses n = liftM sort (tosses n)

tossesWithAtLeastOneHead n = do
  result <- tosses n
  guard (Heads `elem` result)
  return result

{-

Using Control.Monad.Probability:

*Main> ddist (unorderedTosses 2)
[MV 0.25 [Heads,Heads],MV 0.5 [Heads,Tails],MV 0.25 [Tails,Tails]]

Using Control.Monad.Probability.Rational:

*Main> ddist (unorderedTosses 2)
[MV 1%4 [Heads,Heads],MV 1%2 [Heads,Tails],MV 1%4 [Tails,Tails]]

Using either:

*Main> sampleIO (unorderedTosses 2) 10
[[Heads,Heads],[Heads,Heads],[Heads,Heads],[Tails,Tails],[Heads,Heads],[Heads,Heads],[Heads,Tails],[Heads,Tails],[Tails,Tails],[Heads,Tails]]

-}
