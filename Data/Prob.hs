{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright    : 2007 Eric Kidd
License      : BSD3
Stability    : experimental
Portability  : non-portable (newtype deriving)

Support for probability values.
-}

module Data.Prob (Prob(..)) where

-- | A probability is a number between 0 and 1, inclusive.
newtype Prob = Prob Double
  deriving (Eq, Ord, Num, Fractional)

-- Is this how we want to define Show?
instance Show Prob where
  show (Prob p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits :: Int
          digits = round (1000 * p)
          intPart = digits `div` 10
          fracPart = digits `mod` 10
