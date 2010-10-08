{-# LANGUAGE BangPatterns #-}
-- | Euler 21
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.
--

import qualified Data.IntMap as IM
import Data.List (foldl')

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n-1], n `mod` d == 0]


d :: Int -> Int
d = sum . divisors


main = do
  let ds = map (\n -> (n, d n)) [1..10000]
      sumMap = foldl' (\m (n, d) -> IM.insert n d m) IM.empty ds  
      amicableNumbers = filter (\(n, d) -> case IM.lookup d sumMap of
                                              Just s -> s == n
                                              Nothing -> False) ds

  putStrLn $ show amicableNumbers
  putStrLn . show . sum $ map fst $ filter (\(n, d) -> n /= d) amicableNumbers 

