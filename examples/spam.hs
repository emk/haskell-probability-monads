import Control.Monad.Distribution
import Control.Monad.MonoidValue
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Probability

-- Spam Filtering
--
-- Inspired by <http://www.paulgraham.com/spam.html> and
-- <http://www.mathpages.com/home/kmath267.htm>.

-- Each message is spam (junk mail) or "ham" (good mail).
data MsgType = Spam | Ham
  deriving (Show, Eq, Enum, Bounded)

hasWord :: String -> BDDist MsgType ->
           BDDist MsgType
hasWord word prior = do
  msgType <- prior
  wordPresent <- wordPresentIn msgType word
  guard wordPresent
  return msgType

-- > bayes msgTypePrior
-- [Perhaps Spam 64.2%,Perhaps Ham 35.8%]

-- > bayes (hasWord "free" msgTypePrior)
-- [Perhaps Spam 90.5%,Perhaps Ham 9.5%]

wordPresentIn msgType word =
    boolDist (prob (n/total))
  where wordCounts = findWordCounts word
        n     = entryFor msgType wordCounts
        total = entryFor msgType msgCounts

boolDist :: Prob -> BDDist Bool
boolDist p =
    weighted [(True, p'), (False, 1-p')]
  where p' = fromProb p

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

-- > bayes (uniformAll :: BDDist MsgType)
-- [Perhaps Spam 50.0%,Perhaps Ham 50.0%]

characteristic f = f uniformAll

-- > bayes (characteristic (hasWord "free"))
-- [Perhaps Spam 84.1%,Perhaps Ham 15.9%]

score f =
  distance (characteristic f) uniformAll

distance :: (Eq a, Enum a, Bounded a) =>
            BDDist a -> BDDist a -> Double
distance dist1 dist2 =
    sum (map (^2) (zipWith (-) ps1 ps2))
  where ps1 = vectorFromDist dist1
        ps2 = vectorFromDist dist2

vectorFromDist dist =
  map (fromRational . fromProb) (probsFromDist dist)

probsFromDist dist =
    map (\x -> (sumProbs . matching x) (listFromMaybeDist (bayes dist)))
        allValues
  where matching x = filter ((==x) . mvValue)
        sumProbs = sum . map mvMonoid

listFromMaybeDist Nothing = []
listFromMaybeDist (Just dist) = runMVT dist

adjustMinimums xs = map (/ total) adjusted
  where adjusted = map (max 0.01) xs
        total = sum adjusted

adjustedProbsFromDist dist =
  adjustMinimums (probsFromDist dist)

classifierProbs f =
   adjustedProbsFromDist (characteristic f)

--applyProbs :: (Enum a) => [Prob] -> BDDist a -> BDDist a
applyProbs probs prior = do
  msgType <- prior
  applyProb (entryFor msgType probs)
  return msgType

-- Will need LaTeX PNG to explain.
applyProb :: Prob -> BDDist ()
applyProb p = do
  b <- boolDist p
  guard b

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
