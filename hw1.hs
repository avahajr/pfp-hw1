import qualified GHC.TypeLits as time
{-
 Name: Ava Hajratwala
 Uni: ash2261
 Collaborators:
 References:
 -- https://wiki.haskell.org/Lambda_abstraction
 -- I owe everything to LYAH
 ------------------------------
 COMS 4995 001 Parallel Function Programming
 Homework 1
 Due at 11:59 PM Sunday, September 24, 2023
 Modify this file with your solutions and submit it on Courseworks
 You may use functions, etc. from the Standard Prelude, but no
 other libraries
 Do not modify the type signatures for any of the provided functions.
 
 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.
 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.
 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.
 Feel free to add and delete single-line comments (i.e., --)
 Please don't write any separate top-level helper functions.  If you need an
 additional function, make it local to the function or value of the
 problem in a let or where clause.
 -----
 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?
 Use lts-21.9 as the resolver for the Haskell Tool Stack.
 E.g., stack --resolver lts-21.9 ghci
 Your code should load under GHCi 9.4.6 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw1
-}
{- 1) Write a function elem' x xs that returns true if x is an element of the
      list xs.  Assume the list xs is in increasing order but may be infinite.
      E.g,
      *Main> elem' (1::Int) []
      False
      *Main> elem' (57::Int) [1..]
      True
      *Main> elem' (57::Int) [1,3..]
      True
      *Main> elem' (57::Int) [1..100]
      True
      *Main> elem' (58::Int) [1,3..]
      False
-}
elem' :: Ord a => a -> [a] -> Bool
elem' x xs = search xs
      where -- this where clause defines the 'search' function
            search [] = False -- base case
            search (y:ys) -- add y to the ys
                  | x == y = True
                  | x < y = False
                  | otherwise = search ys

{- 2) Write a function for testing the Goldbach conjecture, i.e., that
      every even integer greater than 2 can be written as a sum of two
      primes.  Write a function goldbach n that returns a pair of
      primes (p1, p2) such that p1 + p2 = n.  Use the list primes and
      the elem' function in your solution.  Assume n is in [4,6,...].
      Avoid a O(n^2) search.  Note: the solution often isn't unique;
      you may return any pair of primes that sum to n.
      Hint: my solution was mostly a one-line list comprehension.
      Generate candidate solutions and filter them.
      Example:
      *Main> take 10 [ goldbach n | n <- [4,6..] ]
      [(2,2),(3,3),(3,5),(3,7),(5,7),(3,11),(3,13),(5,13),(3,17),(3,19)]
      *Main> goldbach 2642
      (103,2539)
      *Main> goldbach 65536
      (17,65519)
         
-}
goldbach :: Integer -> (Integer, Integer)
goldbach n = head [(p1,p2) | p1 <- primes, let p2 = n-p1, p2 `elem'` primes]
-- An infinite list of primes; do not modify
primes :: [Integer]
primes = f [2..] where f (p:xs) = p : f [ x | x <- xs, x `mod` p /= 0 ]
                       f [] = []

{- 3) Write a function "maxrun" that reports the length of the longest
      contiguous run of equal values in a list.
      E.g.,
 
      *Main> maxrun ([] :: [Int])
      0
      *Main> maxrun [1 :: Int]
      1
      *Main> maxrun [1 :: Int,1]
      2
      *Main> maxrun "aabbbcdd"
      3
-}
maxrun :: Eq a => [a] -> Int
maxrun [] = 0
maxrun (x:xs) = maxrun' xs x 1 1 
  where
    maxrun' :: Eq a => [a] -> a -> Int -> Int -> Int
    maxrun' [] _ _ currentMax = currentMax
    maxrun' (y:ys) prevEle currRun currMax
      | y == prevEle = maxrun' ys y (currRun + 1) currMax
      | otherwise = maxrun' ys y 1 (max currRun currMax)



{- 4) Write the infinite list of Pell numbers in which the next number is
      twice the previous number plus the the number before that.
      Hint: declare the list recursively in terms of itself.  My solution is
      a single line.      
      Example:
      *Main> take 10 pell
      [0,1,2,5,12,29,70,169,408,985]
-} 
pell :: [Integer] 
pell = 0 : 1 : zipWith (\a b -> 2 * b + a) pell (tail pell)


{- 5) Use the list of Pell numbers to construct a list of successive
      approximations to the square root of 2, that
      is, a compute a list derived from the Pell numbers whose entries are
      P    + P
       n-1    n
     -----------
          P
           n
     and verify that it approaches the square root of 2: 1.4142135623730951
     Hint: I converted the Integer Pell numbers using fromIntegral.
     My solution was two lines
     Example:
     *Main> take 6 sqrt2
     [1.0,1.5,1.4,1.4166666666666667,1.4137931034482758,1.4142857142857144]
-}
sqrt2 :: [Double]
sqrt2 = zipWith (\p1 p2 -> (p1+p2)/p2) pell' (tail pell')
      where pell' = map fromIntegral pell
-- tail returns everything but the head, zipping with the original list creates an 
-- 'offset' which allows us to reference both n and n-1 elements at the same time.


{- 6) Write a digsum function that reports the sum of the digits of
      an integer.  Negative inputs should report a negative sum.
      Use the quotRem function, which is defined for the Integral type class,
      to pick apart the number into digits.
      Do better than the solutions on Stack Overflow (most of which use
      non-Prelude functions, anyway):
      https://stackoverflow.com/questions/2838727/how-do-i-get-the-sums-of-the-digits-of-a-large-number-in-haskell
      Examples:
      *Main> digsum (0 :: Int)
      0
      *Main> digsum (123 :: Int)
      6
      *Main> digsum ((-123) :: Int)
      -6
      *Main> digsum (123456789 :: Integer)
      45
      *Main> digsum (100010000 :: Integer)
      2
-}
digsum :: Integral a => a -> a
digsum _ = 0 -- Change this

{- 7) Generate the rows of Pascal's triangle as an infinite list of lists.
      Hint: use zipWith, tail, and list comprenensions.  As an intermediate
      step, try writing a function that takes one line of the triangle
      and produces the next.  My final solution is a single line.
    Example:
    *Main> take 7 (pascal :: [[Int]])
    [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1],[1,6,15,20,15,6,1]]
    
-}
pascal :: Num a => [[a]]
pascal = [] -- Change this
