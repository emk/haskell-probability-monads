{-# LANGUAGE MultiParamTypeClasses #-}

-- Standard modules.
import Control.Monad
import Control.Monad.Trans
import Data.List
import qualified Data.Map as M
import Data.Maybe
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
-- Spam Filtering
--
-- Inspired by <http://www.paulgraham.com/spam.html> and
-- <http://www.mathpages.com/home/kmath267.htm>.

-- Each message is spam (junk mail) or "ham" (good mail).
data MsgType = Spam | Ham
  deriving (Show, Eq, Enum, Bounded)

hasWord :: String -> FDist' MsgType ->
           FDist' MsgType
hasWord word prior = do
  msgType <- prior
  wordPresent <- wordPresentIn msgType word
  condition wordPresent
  return msgType

-- > bayes msgTypePrior
-- [Perhaps Spam 64.2%,Perhaps Ham 35.8%]

-- > bayes (hasWord "free" msgTypePrior)
-- [Perhaps Spam 90.5%,Perhaps Ham 9.5%]

wordPresentIn msgType word =
    boolDist (Prob (n/total))
  where wordCounts = findWordCounts word
        n     = entryFor msgType wordCounts
        total = entryFor msgType msgCounts

boolDist :: Prob -> FDist' Bool
boolDist (Prob p) =
    weighted [(True, p), (False, 1-p)]

msgCounts = [102, 57]

wordCountTable =
  M.fromList [("free", [57, 6]),
              -- Lots of words...
              ("bayes", [1, 10]),
              ("monad", [0, 22])]

entryFor :: Enum a => a -> [b] -> b
entryFor x ys = ys !! fromEnum x

findWordCounts word =
  M.findWithDefault [0,0] word wordCountTable

msgTypePrior :: Dist d => d MsgType
msgTypePrior =
  weighted (zipWith (,) [Spam,Ham] msgCounts)

-- > bayes (hasWord "bayes" msgTypePrior)
-- [Perhaps Spam 9.1%,Perhaps Ham 90.9%]

hasWords []     prior = prior
hasWords (w:ws) prior = do
  hasWord w (hasWords ws prior)

-- > bayes (hasWords ["free","bayes"] msgTypePrior)
-- [Perhaps Spam 34.7%,Perhaps Ham 65.3%]

uniformAll :: (Dist d,Enum a,Bounded a) => d a
uniformAll = uniform allValues

allValues :: (Enum a,Bounded a) => [a]
allValues = enumFromTo minBound maxBound

-- > bayes (uniformAll :: FDist' MsgType)
-- [Perhaps Spam 50.0%,Perhaps Ham 50.0%]

characteristic f = f uniformAll

-- > bayes (characteristic (hasWord "free"))
-- [Perhaps Spam 84.1%,Perhaps Ham 15.9%]

score f =
  distance (characteristic f) uniformAll

distance :: (Eq a, Enum a, Bounded a) =>
            FDist' a -> FDist' a -> Double
distance dist1 dist2 =
    sum (map (^2) (zipWith (-) ps1 ps2))
  where ps1 = vectorFromDist dist1
        ps2 = vectorFromDist dist2

vectorFromDist dist =
  map doubleFromProb (probsFromDist dist)

probsFromDist dist =
    map (\x -> (sumProbs . matching x) (bayes dist))
        allValues
  where matching x = filter ((==x) . perhapsValue)
        sumProbs = sum . map perhapsProb

adjustMinimums xs = map (/ total) adjusted
  where adjusted = map (max 0.01) xs
        total = sum adjusted

adjustedProbsFromDist dist =
  adjustMinimums (probsFromDist dist)

classifierProbs f =
   adjustedProbsFromDist (characteristic f)

--applyProbs :: (Enum a) => [Prob] -> FDist' a -> FDist' a
applyProbs probs prior = do
  msgType <- prior
  applyProb (entryFor msgType probs)
  return msgType

-- Will need LaTeX PNG to explain.
applyProb :: Prob -> FDist' ()
applyProb p = do
  b <- boolDist p
  condition b

-- > bayes (hasWord "free" msgTypePrior)
-- [Perhaps Spam 90.5%,Perhaps Ham 9.5%]
-- > let probs = classifierProbs (hasWord "free")
-- > bayes (applyProbs probs msgTypePrior)
-- [Perhaps Spam 90.5%,Perhaps Ham 9.5%]

data Classifier = Classifier Double [Prob]
  deriving Show

classifier f = Classifier (score f) (classifierProbs f)

applyClassifier (Classifier _ probs) =
  applyProbs probs

instance Eq Classifier where
  (Classifier s1 _) == (Classifier s2 _) =
      s1 == s2

instance Ord Classifier where
  compare (Classifier s1 _)
          (Classifier s2 _) =
    compare s2 s1

-- > classifier (hasWord "free")
-- Classifier 0.23 [84.1%,15.9%]

classifiers :: M.Map String Classifier
classifiers =
    M.mapWithKey toClassifier wordCountTable
  where toClassifier w _ =
          classifier (hasWord w)

findClassifier :: String -> Maybe Classifier
findClassifier w = M.lookup w classifiers

findClassifiers n ws = 
    take n (sort classifiers)
  where classifiers =
          catMaybes (map findClassifier ws)

hasTokens ws prior =
  foldr applyClassifier
        prior
        (findClassifiers 15 ws)

-- > bayes (hasTokens ["bayes", "free"]
--                    msgTypePrior)
-- [Perhaps Spam 34.7%,Perhaps Ham 65.3%]


-- ========================================================================
-- Robot localization
--
-- Example based on "Bayesian Filters for Location Estimation", Fox et al.,
-- 2005.  Available online at:
--
-- http://seattle.intel-research.net/people/jhightower/pubs/fox2003bayesian/fox2003bayesian.pdf

-- The hallway extends from 0 to 299, and
-- it contains three doors.
doorAtPosition :: Int -> Bool
doorAtPosition pos
    -- Doors 1, 2 and 3.
    |  26 <= pos && pos <  58 = True
    |  82 <= pos && pos < 114 = True
    | 192 <= pos && pos < 224 = True
    | otherwise        = False

localizeRobot :: WPS Int
localizeRobot = do
  -- Pick a random starting location
  -- to use as a hypothesis.
  pos1 <- uniform [0..299]
  -- We know we're at a door.  Hypotheses
  -- which agree with this fact get a
  -- weight of 1, others get 0.
  if doorAtPosition pos1
    then weight 1
    else weight 0

  -- Drive forward a bit.
  let pos2 = pos1 + 28
  -- We know we're not at a door.
  if not (doorAtPosition pos2)
    then weight 1
    else weight 0

  -- Drive forward some more.
  let pos3 = pos2 + 28
  if doorAtPosition pos3
    then weight 1
    else weight 0
  -- Our final hypothesis.
  return pos3

-- > runRand (runWPS localizeRobot 10)
-- [Perhaps 106 100.0%,
--  never,never,never,never,never,
--  Perhaps 93 100.0%,
--  never,never,never]

-- > runWPS' localizeRobot 10
-- [97,109,93]


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
catMaybes' (Perhaps _ 0 : xs) =
  catMaybes' xs
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


-- ========================================================================
-- Particle System

newtype PS a = PS { runPS :: Int -> Rand [a] }

liftRand :: Rand a -> PS a
liftRand r = PS (sample r)

instance Functor PS where
  fmap f ps = PS mapped
    where mapped n =
            liftM (map f) (runPS ps n)

instance Monad PS where
  return = liftRand . return
  ps >>= f = joinPS (fmap f ps)

joinPS :: PS (PS a) -> PS a
joinPS psps = PS (joinPS' psps)

joinPS' :: PS (PS a) -> Int -> Rand [a]
joinPS' psps n = do
    pss <- (runPS psps n)
    xs <- sequence (map sample1 pss)
    return (concat xs) -- TODO: Can we base on Rand's join?
  where sample1 ps = runPS ps 1

instance Dist PS where
  weighted = liftRand . weighted

type WPS = PerhapsT PS

instance Dist (PerhapsT PS) where
  weighted = PerhapsT . weighted . map liftWeighted
    where liftWeighted (x,w) = (Perhaps x 1,w)

weight :: Prob -> WPS ()
weight p = PerhapsT (return (Perhaps () p))

runWPS wps n = runPS (runPerhapsT wps) n

runWPS' wps n = (runRand . liftM catPossible) (runWPS wps n)

catPossible (ph:phs) | impossible ph =
  catPossible phs
catPossible (Perhaps x p:phs) =
  x:(catPossible phs)
catPossible [] = []
