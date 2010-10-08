{-# LANGUAGE BangPatterns #-}

-- | Euler problem 50
-- The prime 41, can be written as the sum of six consecutive primes:
--
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
-- This is the longest sum of consecutive primes that adds to a prime below one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most consecutive primes?
--

{- This code was shamelessly stolen from the Haskell Wiki -}


import Data.List (sortBy, foldl')
import Data.Maybe
import Data.Ord (comparing)
import System.Environment

import ONeillPrimes (primesToLimit)


euler10 :: Integer
euler10  = sum $ primesToLimit 2000000


main = do
  putStrLn . show $ euler10 
