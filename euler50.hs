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


-- | partition the list into two lists: the first are the items for which the 
-- accumulation under the function holds, the second the remainder of the
-- elements
partitionF :: (a -> a -> (a,Bool)) -- ^ ending condition
           -> a                    -- ^ initial value
           -> [a]                  -- ^ list of items to check
           -> ([a], [a])           -- ^ partitions
partitionF _ _ [] = ([], [])
partitionF f !y (x:xs) = p f y (x:xs) []
  where p _ _ [] zs = (zs, [])
        p f !y (x:xs) !zs = case f y x of
                              (_, False) -> (zs, x:xs)
                              (!y', True) -> p f y' xs (x:zs)

-- | Is the value p to be written as a sum of consecutive values from ps?
isSum :: [Int] -> Int -> (Int,Bool)
isSum ps p =
  let (xs, zs) = partitionF (\x y -> let s = x+y in (s, s <= p)) 0 ps
  in isSum' p ([], 0, xs, length xs) zs 

isSum' :: Int -> ([Int], Int, [Int], Int) -> [Int] -> (Int, Bool)
isSum' p (xs, lx, ys, ly) [] = if sum xs + sum ys == p then let zs = xs ++ ys in (lx + ly , True) else (0,False)
isSum' p ([], _, ys, ly) zs = isSum' p (reverse ys, ly, [], 0) zs
isSum' p (xss@(x:xs), lx, yss, ly) zss@(z:zs) 
  | s > p     = isSum' p (xs, lx - 1, yss, ly) zss -- drop the smallest item
  | s == p    = (lx + ly, True) 
  | otherwise = if z > sx then isSum' p ([], 0, z:yss, ly+1) zs
                          else isSum' p (xss, lx, z:yss, ly+1) zs
  where sx = sum xss
        sy = sum yss 
        s = sx + sy
{-
if (sxs + y) <= p 
       then isSum' p (xs ++ [y]) ys
       else isSum' p xs yss
-}

euler50 :: Int -> (Int, Int)
euler50 m = check (2,5) primes
  where primes = reverse $ primesToLimit m 
        check :: (Int, Int) -> [Int] -> (Int, Int)
        check c [] = c
        check c@(!l0,!p0) (p:ps) = case isSum (reverse ps) p of
                                          (l', True) -> if l' > l0 then check (l',p) ps else check (l0,p0) ps
                                          (_, False)  -> check (l0,p0) ps

{-
        check c@(!ps0,!l0,!p0) p = case isSum primes p of 
                                    (ps, True) -> let l = length ps in if l > l0 then (ps,l,p) else (ps0,l0,p0) 
                                    (_, False) -> c
-}


main = do
  args <- getArgs

  let m = read $ args !! 0 :: Int
      ps = euler50 m
  putStrLn $ "max = " ++ show m
  putStrLn $ show ps
