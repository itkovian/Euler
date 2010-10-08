{-# LANGUAGE BangPatterns #-}
-- | The following iterative sequence is defined for the set of positive integers:
--
-- n  n/2 (n is even)
-- n  3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--
-- 13  40  20  10  5  16  8  4  2  1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--

import qualified Data.Map as M
import Data.List(foldl')
import System.Environment(getArgs)


chain :: M.Map Integer Int -> Integer -> M.Map Integer Int
chain m orig = chain' m orig 0 []
  where chain' :: M.Map Integer Int -- ^ Current map with chain lengths
               -> Integer           -- ^ Current integer for which a chain (remainder) must be determined
               -> Int               -- ^ Current length
               -> [Integer]         -- ^ Chain so far (reversed)
               -> M.Map Integer Int 
        chain' m current l cs = case M.lookup current m of 
                                   Just c' -> updateChain m cs (c'+1)
                                   Nothing -> chain' m next (l+1) (current:cs)
          where next 
                 | current `mod` 2 == 0 = current `div` 2
                 | otherwise            = 3 * current + 1

        updateChain :: M.Map Integer Int -> [Integer] -> Int -> M.Map Integer Int
        updateChain m cs !l = foldl' (\m (v,k) -> M.insert k v m) m $ filter (\(v, k) -> k < 1000000) $ zip [l..] cs  


euler13 :: Integer -> (Int,Integer)
euler13 m = 
  let chainMap = foldl' chain (M.insert 1 1 M.empty) [2 .. m]
  in M.foldWithKey (\key v (l, k) -> if v > l then (v,key) else (l, k)) (1,1) chainMap 



main = do
  args <- getArgs
  let m = read $ args !! 0 :: Integer
  putStrLn . show $ euler13 m
  --putStrLn . show $ foldl' chain (M.insert 1 1 M.empty) [2,3,13]
