{-# LANGUAGE NoMonomorphismRestriction #-}

-- See <http://www.randomhacks.net/articles/2007/02/21/randomly-sampled-distributions>.

import Control.Monad.Distribution

data Child = Girl | Boy
  deriving (Show, Eq, Ord)

child = uniform [Girl, Boy]

family = do
  child1 <- child
  child2 <- child
  return [child1, child2]
