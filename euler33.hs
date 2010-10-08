import Data.List


cancel :: (Char, Char) -> (Char, Char) -> (Char, Char)
cancel (a,b) (c,d)
  | a == d 


euler33 = [ (show x, show y) | x <- [10 .. 99], y <- [10..99], intersect (show x) (show y) /= []] in filter (not . (\(_,(c,d)) -> c == "0" || d == "0" || c == "1" || d == "1" || c == d)) . nubBy (\(a,b) (c,d) -> b == d) . sort $ map (\(x,y) -> let (i:_) = intersect x y in ((x,y),(delete i x, delete i y))) xs 





main = do
          putStrLn $ show euler33
