{-# LANGUAGE TemplateHaskell #-}
-- needed for running all tests

import PartSum -- test this module
import Test.QuickCheck -- for writing tests
import Test.QuickCheck.All -- for running all tests starting with prop_
import Data.List (nub, sort)

runTests :: IO Bool
-- runTests = $verboseCheckAll -- for verbose test
runTests = $quickCheckAll  -- for mute test

main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."
		
-- generate number in (0, n)		
choose_n n = choose (0, n) 

-- generate summand list of positive, unique integers
choose_xi c = do 
	-- generate arbitrary integer list of length <= c
	ls <- (resize c arbitrary) :: Gen [Int] 
	-- for each number take abs value, add 1 and remove duplicates
	return $ nub $ map ((+1).abs) ls 
	
newtype MyTest = MyTest (Int, [Int]) deriving Show

instance Arbitrary MyTest where
	arbitrary = do
		n <- choose_n 12 
		xi <- choose_xi 5
		return $ MyTest (n,xi)
		
-- test with: sample' arbitrary :: IO [MyTest]

-- check sum 
prop_sum (MyTest (n,xs)) = 
	all p ls where 
	p = \si -> sum si == n     -- ((==) n).sum
	ls = partSum n xs
		
-- check for no duplicates
prop_no_duplicates (MyTest (n,xs)) = ls == nub ls where 
	ls = partSum n xs

-- check all summands of solution are in xs
prop_sum_val (MyTest (n,xs)) = all p ls where
	p = \si -> all p2 si
	p2 = \s -> elem s xs
	ls = partSum n xs
	
-- check sum 2	
prop_sum2 (MyTest (n,xs)) = 
	collect (length xs)$
	collect (length ls)$
	all p ls where 
	p = \si -> sum si == n     -- p = ((==) n).sum
	ls = partSum2 n xs

-- check for no duplicates 
prop_no_duplicates2 (MyTest (n,xs)) = ls == nub ls where 
	ls = map sort ls2 -- sort each solutions
	ls2 = partSum2 n xs

-- check all summands of solution are in xs 2
prop_sum_val2 (MyTest (n,xs)) = all p ls where
	p = \si -> all p2 si
	p2 = \s -> elem s xs
	ls = partSum n xs
	
	