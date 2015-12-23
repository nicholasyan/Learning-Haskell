{-# OPTIONS_GHC -Wall #-}

module CreditCardValidator where

-- CIS 194: Homework 1
-- Nicholas Yan
-- December 22, 2015
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- EXERCISE 1

-- convert positive integers to a list of digits
-- e.g. toDigits (1234) == [1, 2, 3, 4]
--      toDigits (-700) == [] 

toDigits :: Integer -> [Integer]
toDigits x      
    | x == 0        = []
    | x < 0         = []
    | x < 10        = [x]
    | otherwise     = toDigits (x `div` 10) ++ [x `mod` 10] 

-- convert positive integers to a list of digits, but with the digits reversed
-- e.g. toDigitsRev (1234) == [4, 3, 2, 1]
-- (reversed version of toDigits)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x == 0        = []
    | x < 0         = []
    | x < 10        = [x]
    | otherwise     = [x `mod` 10] ++ toDigitsRev (x `div` 10)

-- EXERCISE 2

-- double every other integer in a list beginning from the right
-- e.g. doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherLeft(reverse xs))

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft []         = []
doubleEveryOtherLeft (x:[])     = [x]
doubleEveryOtherLeft (x:y:xs)   = [x, y * 2] ++ doubleEveryOtherLeft(xs)

-- EXERCISE 3

-- calculates the sum of digits in a list
-- e.g. sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:[]) = x 
sumDigits (x:xs) = sumDigits(toDigits(x)) + sumDigits(xs)

-- EXERCISE 4

-- indicates whether a given number can be a valid credit card number
-- a valid credit card number is a number where after:
--
--      1) every digit in it (from the right) is doubled
--      2) all the remaining digits are summed
--      3) the sum is modded by 10
--
-- the result is equal to 0
--
-- e.g. validate 4012888888881881 = True
--      validate 4012888888881882 = False
 
validate :: Integer -> Bool
validate cc
    | sumDigits(doubleEveryOther(toDigits(cc))) `mod` 10 == 0   = True
    | otherwise                                                 = False
