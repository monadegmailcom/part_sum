module PartSum (
   partSum,
	partSum2
) where

{- Partition einer Summe unter Berücksichtigung der Reihenfolge
given 
n >= 0 and 
(ki) with ki != kj for i!=j and ki > 0) 
eval all (xi) with n = sum xi and xi in (ki) 
example:
partSum 8 [2,3]
[[2,2,2,2],[3,3,2],[3,2,3],[2,3,3]]
sample algorithm
n = 8, ki = [2,3]
(8 [])                      -- (n (xi))
(6 [2])                                         (5 [3]) -- all possibities for x1
(4 [2,2])               (3 [3,2])               (3 [2,3])               (2 [3,3])
(2 [2,2,2]) (1 [3,2,2]) (1 [2,3,2]) (0 [3,3,2]) (1 [2,2,3]) (0 [3,2,3]) (0 [2,3,3]) (-1 [3,3,3])
[2,2,2,2] x x [3,3,2] x [3,2,3] [2,3,3] x
-}
partSum :: Int -> [Int] -> [[Int]]
partSum n ks =
   f n [] []
   where
      -- params n, (xi), solution accumulation
      f :: Int -> [Int] -> [[Int]] -> [[Int]]
      f s ls acc
         | s < 0     = acc    -- no solution to add
         | s == 0    = ls:acc -- add solution
         | otherwise = foldr (\k as -> f (s-k) (k:ls) as) acc ks -- recursive call for all ki

{- Partition einer Summe ohne Berücksichtigung der Reihenfolge
given 
n >= 0 and 
(ki) with ki != kj for i!=j and ki > 0) 
eval all {xi} with n = sum xi and xi in (ki) 
example:
partSum2 12 [2,3]
[[3,3,3,3],[3,3,2,2,2],[2,2,2,2,2,2]]
-}
			
partSum2 :: Int -> [Int] -> [[Int]] 
partSum2 n ks = 
   f n [] ks [] 
   where
      -- params n, (xi), (ki), solution accumulation
      f :: Int -> [Int] -> [Int] -> [[Int]] -> [[Int]]
      f 0 ls _ acc = ls:acc -- add solution
      f _ _ [] acc = acc -- no solution to add
      f s ls l@(k:ks) acc
         | s < 0     = acc    -- no solution to add
         | otherwise = f s ls ks acc2
            where acc2 = f (s-k) (k:ls) l acc