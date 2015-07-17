import Control.Applicative

import GHC.Real

import Data.Probability
import Control.Monad.Distribution

import Control.Monad.MonoidValue

-- import Spam.SpamClassifier 
import Spam.PPrint 
import Spam.MailProcessor 
import Spam.SpamClassifier

import System.Directory (getDirectoryContents)

import qualified Data.BST as BST

main :: IO ()
main = clasifyEmail mailPath

boolDist :: Prob -> BDDist Bool
boolDist p =
    weighted [(True, p'), (False, 1-p')]
  where p' = fromProb p


entryFor :: Enum a => a -> [b] -> b
entryFor x ys = ys !! fromEnum x


mailPath = "mail.txt"

clasifyEmail :: String -> IO ()
clasifyEmail path = do 
  emailwords     <- processEmail <$> readFile path
  wordCountTable <- makeCountTable 
  let res = bayes (hasWords wordCountTable emailwords initmsgType)
  print $ pprint res 


spamDir = "Spam/spamEmails/"
hamDir = "Spam/hamEmails/"

makeCountTable 
  = do spamWords <- getWordsFromDir spamDir
       hamWords  <- getWordsFromDir hamDir
       return $ updateWords updateHam  [0, 1] hamWords 
              $ updateWords updateSpam [1, 0] spamWords BST.empty
  where
  	updateSpam [s, h] = [s+1, h]
  	updateHam  [s, h] = [s, h+1]

updateWords f d ls m = foldr (\k m -> BST.insertWith g k d m) m ls 
  where
    g _ = f 

getWordsFromDir dir 
  = do files <- map (dir ++) <$> filter validFile <$> getDirectoryContents dir
       cts   <- mapM readFile files
       return $ concatMap processEmail cts 
  where
  	validFile x = x /= "." && x /= ".."


-- | initCounts the initial spam and ham email count 
initCounts :: [Ratio Integer]
initCounts = [102, 57]


initmsgType :: Dist d => d MsgType
initmsgType =
  weighted (zipWith (,) [Spam,Ham] initCounts)



hasWord wc word prior = do
  msgType <- prior
  wordPresent <- wordPresentIn wc msgType word
  guard wordPresent
  return msgType


wordPresentIn wc msgType word =
    boolDist (prob (n/total))
  where wordCounts = findWordCounts wc word
        n     = entryFor msgType wordCounts
        total = entryFor msgType initCounts


findWordCounts wordCountTable word =
  BST.findWithDefault [1,1] word wordCountTable


hasWords _ []     prior = prior
hasWords wc (w:ws) prior = do
  hasWord wc  w (hasWords wc ws prior)


