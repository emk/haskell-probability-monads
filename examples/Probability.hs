{-# LANGUAGE MultiParamTypeClasses #-}

-- Standard modules.
import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Random

-- Modules provided by this library.
import Control.Monad.Dist
import Control.Monad.Maybe
import Control.Monad.Perhaps
import Data.Prob


-- ========================================================================
-- Example 1: Family
-- See <http://www.randomhacks.net/articles/2007/02/21/randomly-sampled-distributions>.

data Child = Girl | Boy
  deriving (Show, Eq, Ord)

child :: Dist d => d Child
child = uniform [Girl, Boy]

family :: Dist d => d [Child]
family = do
  child1 <- child
  child2 <- child
  return [child1, child2]

-- exact family
-- fmap histogram (sampleIO family 1000)


-- ========================================================================
-- Example 2: Drug Testing
-- See <http://www.randomhacks.net/articles/2007/02/22/bayes-rule-and-drug-tests>.

data HeroinStatus = User | Clean
  deriving (Show, Eq)

data Test = Pos | Neg
  deriving (Show, Eq)

percent p x1 x2 =
  weighted [(x1, p), (x2, 100-p)]

percentUser p = percent p User Clean
percentPos p = percent p Pos Neg

drugTest1 :: Dist d => d (HeroinStatus, Test)
drugTest1 = do
  heroinStatus <- percentUser 0.1
  testResult <-
    if heroinStatus == User
      then percentPos 99
      else percentPos 1
  return (heroinStatus, testResult)

-- > exact drugTest1
-- [Perhaps (User,Pos) 0.1%,
--  Perhaps (User,Neg) 0.0%,
--  Perhaps (Clean,Pos) 1.0%,
--  Perhaps (Clean,Neg) 98.9%]

drugTest2 :: Dist d => d (Maybe HeroinStatus)
drugTest2 = do
  (heroinStatus, testResult) <- drugTest1
  return (if testResult == Pos
            then Just heroinStatus
            else Nothing)

-- > exact drugTest2
-- [Perhaps (Just User) 0.1%,
--  Perhaps Nothing 0.0%,
--  Perhaps (Just Clean) 1.0%,
--  Perhaps Nothing 98.9%]

-- > exact (onlyJust drugTest2)
-- [Perhaps User 9.0%,Perhaps Clean 91.0%]

drugTest3 :: FDist' HeroinStatus ->
             FDist' HeroinStatus
drugTest3 prior = do
  heroinStatus <- prior
  testResult <-
    if heroinStatus == User
      then percentPos 99
      else percentPos 1
  condition (testResult == Pos)
  return heroinStatus

-- > bayes (drugTest3 (percentUser 0.1))
-- [Perhaps User 9.0%,Perhaps Clean 91.0%]
-- > bayes (drugTest3 (percentUser 50))
-- [Perhaps User 99.0%,Perhaps Clean 1.0%]


-- ========================================================================
-- Finite distributions
--
-- Heavily inspired by Martin Erwig's and Steve Kollmansberger's
-- /Probabilistic Functional Programming/, which can be found at
-- <http://web.engr.oregonstate.edu/~erwig/pfp/>.
--
-- See <http://www.randomhacks.net/articles/2007/02/21/refactoring-probability-distributions>.

type FDist = PerhapsT ([])

instance Dist FDist where
  weighted [] = error "Empty distribution"
  weighted xws = PerhapsT (map weight xws)
    where weight (x,w) = Perhaps x (Prob (w / sum))
          sum = foldl' (+) 0 (map snd xws)

exact :: FDist a -> [Perhaps a]
exact = runPerhapsT


-- ========================================================================
-- Finite distributions with Bayesian inference
--
-- See <http://www.randomhacks.net/articles/2007/02/22/bayes-rule-and-drug-tests>.

type FDist' = MaybeT FDist

instance Dist FDist' where
  weighted xws = lift (weighted xws)

bayes :: FDist' a -> [Perhaps a]
bayes = exact . onlyJust . runMaybeT

condition :: Bool -> FDist' ()
condition = MaybeT . return . toMaybe
  where toMaybe True  = Just ()
        toMaybe False = Nothing

onlyJust :: FDist (Maybe a) -> FDist a
onlyJust dist
    | total > 0 = PerhapsT (map adjust filtered)
    | otherwise = PerhapsT []
  where filtered = catMaybes' (runPerhapsT dist)
        total = sum (map perhapsProb filtered)
        adjust (Perhaps x p) =
          Perhaps x (p / total)

catMaybes' :: [Perhaps (Maybe a)] -> [Perhaps a]
catMaybes' [] = []
catMaybes' (Perhaps Nothing _ : xs) =
  catMaybes' xs
catMaybes' (Perhaps (Just x) p : xs) =
  Perhaps x p : catMaybes' xs


-- ========================================================================
-- Random sampling
--
-- Heavily inspired by Sungwoo Park and colleagues' $\lambda_{\bigcirc}$
-- caculus <http://citeseer.ist.psu.edu/752237.html>.
--
-- See <http://www.randomhacks.net/articles/2007/02/21/randomly-sampled-distributions>.

newtype Rand a = Rand { runRand :: IO a }

randomDouble :: Rand Double
randomDouble = Rand (getStdRandom (random))

instance Functor Rand where
  fmap = liftM

instance Monad Rand where
  return x = Rand (return x)
  r >>= f = Rand (do x <- runRand r
                     runRand (f x))

instance Dist Rand where
  weighted = liftF . weighted

liftF :: FDist a -> Rand a
liftF fdist = do
  n <- randomDouble
  pick (Prob n) (runPerhapsT fdist)

pick :: Monad m => Prob -> [Perhaps a] -> m a
pick _ [] = fail "No values to pick from"
pick n ((Perhaps x p):ps)
  | n <= p    = return x
  | otherwise = pick (n-p) ps

sample :: Rand a -> Int -> Rand [a]
sample r n = sequence (replicate n r)

sampleIO r n = runRand (sample r n)

histogram :: Ord a => [a] -> [Int]
histogram = map length . group . sort
