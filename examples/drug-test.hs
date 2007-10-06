{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Distribution

-- See <http://www.randomhacks.net/articles/2007/02/22/bayes-rule-and-drug-tests>.

data HeroinStatus = User | Clean
  deriving (Show, Eq)

data Test = Pos | Neg
  deriving (Show, Eq)

percent p x1 x2 =
  weighted [(x1, p), (x2, 100-p)]

percentUser p = percent p User Clean
percentPos p = percent p Pos Neg

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

drugTest3 prior = do
  heroinStatus <- prior
  testResult <-
    if heroinStatus == User
      then percentPos 99
      else percentPos 1
  guard (testResult == Pos)
  return heroinStatus

-- > bayes (drugTest3 (percentUser 0.1))
-- [Perhaps User 9.0%,Perhaps Clean 91.0%]
-- > bayes (drugTest3 (percentUser 50))
-- [Perhaps User 99.0%,Perhaps Clean 1.0%]
