
primes = sieve [2 ..]
  where sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0]


main = do
  let p = head . drop 10000 $ primes
  putStrLn $ show p
